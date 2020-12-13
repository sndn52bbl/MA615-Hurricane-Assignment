library(tidyverse)
library(shiny)

# the hurricaneexposure package
## here is the package's Github page: https://github.com/geanders/hurricaneexposure
## here is the description document: https://cran.r-project.org/web/packages/hurricaneexposure/hurricaneexposure.pdf
## to see more details about the hurricaneexposure package: https://cran.r-project.org/web/packages/hurricaneexposure/vignettes/hurricaneexposure.html

# download the hurricaneexposure package
library(drat)
addRepo("geanders")
#install.packages("hurricaneexposuredata")
#library(hurricaneexposuredata)

# use 2 datasets in the package
# attach the hurr_tracks data

data("hurr_tracks")
data("rain")
data_ht <- as.data.frame(hurr_tracks)
# add a column "year"
data_ht$year <- substr(data_ht$date, 1, 4)

# attach the rain data
data_rain <- as.data.frame(rain)


ui <- fluidPage(
    title = "Examples of Data Tables",
    sidebarLayout(
        tabsetPanel(
            conditionalPanel(
                'input.dataset === "data_ht"'),
            
            conditionalPanel(
                'input.dataset === "data_rain"',
            )
        ),
        mainPanel(
            tabsetPanel(
                id = 'dataset',
                tabPanel("Storm tracks for Atlantic basin storms data table",
                
                # Create a new Row in the UI for selectInputs
                fluidRow(
                    column(4,
                           selectInput("storm",
                                       "Storm ID:",
                                       c("All",
                                         unique(as.character(data_ht$storm_id))))
                    ),
                    column(4,
                           selectInput("year",
                                       "Year:",
                                       c("All",
                                         unique(data_ht$year)))
                    )
                ),
                # Create a new row for the table.
                DT::dataTableOutput("table1"),
                textOutput("text1"),
                dataTableOutput("wind")
                
                ),
              
                
                tabPanel("Rainfall for US counties during Atlantic basin tropical storms data table",
                
                # Create a new Row in the UI for selectInputs
                fluidRow(
                    column(4,
                           selectInput("storm",
                                       "Storm ID:",
                                       c("All",
                                         unique(as.character(data_rain$storm_id))))
                    ),
                    column(4,
                           selectInput("fips",
                                       "Fips:",
                                       c("All",
                                         unique(data_rain$fips)))
                    )
                ),
                # Create a new row for the table.
                DT::dataTableOutput("table2")),
                textOutput("text2"),
                dataTableOutput("precip"))
            )
        )
    )


server <- function(input, output) {
    
    # Filter data based on selections

    
    output$table1 <- DT::renderDataTable(DT::datatable({
        data <- data_ht
        if (input$storm != "All") {
            data <- data[data$storm_id == input$storm,]
        }
        if (input$year != "All") {
            data <- data[data$year == input$year,]
        }
        
        data
    }))
    
    output$text1<- renderText({"The following chart shows the average wind scale of each storm arraging from low to high"})
    
    output$wind<- renderDataTable({
        data2<- data.frame(storm_id=hurr_tracks$storm_id, wind=hurr_tracks$wind)
        data2 %>% group_by(storm_id) %>% summarise(wind=mean(wind)) %>% arrange(wind)
    })
    
    # sorted columns are colored now because CSS are attached to them
    # Filter data based on selections
    
    
    output$table2 <- DT::renderDataTable(DT::datatable({
        data2 <- data_rain
        if (input$storm != "All") {
            data2 <- data2[data_rain$storm_id == input$storm,]
        }
        if (input$fips != "All") {
            data2 <- data2[data_rain$fips == input$fips,]
        }
        
        data2
    }))
    
    output$text2<- renderText({"The following chart shows the sum of avrage daily precipitation of each storm arraging from low to high"})
    output$precip<- renderDataTable({
        
        data3<- data.frame(storm_id=rain$storm_id, precip=rain$precip)
        data3 %>% group_by(storm_id) %>% summarize(precip=sum(precip)) %>% arrange(precip)
    })

    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
