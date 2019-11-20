#set wd

# Library 
library(zoo)
library(dplyr)
library(xts)
library(shiny)
library(dygraphs)
library(forecast)
library(highcharter)

# Dygraph Dataframe (using zoo, xts)
data <- read.zoo("FED-SVENY.csv", header = TRUE, sep = ",", FUN = as.Date)
data.xts <- as.xts(data)

data.xts1 <- data.xts["1989/2019"]
head(data.xts1)


# Forecast Dataframe (using zoo, ts)
data <- read.zoo("FED-SVENY.csv", header = TRUE, sep = ",", FUN = as.Date)
data.agg <- as.zooreg(aggregate(data, as.yearmon, mean), freq = 12)


##################### SHINY APP #######################


# UI 

ui<-fluidPage(
  
  titlePanel("U.S. Treasury Yields"),
  
  
  sidebarLayout(
    
    
    sidebarPanel(

      selectInput("bond", label = "Bonds of Varying Maturity",
                  choices = colnames(data.xts1),
                  selected = c("SVENY10","SVENY02"),
                  multiple = TRUE),
      
      selectInput("periods", label = "Forecasting Period",
                  choices = list("1" = 1, 
                                 "2" = 2, 
                                 "3" = 3,
                                 "4" = 4,
                                 "5" = 5,
                                 "6" = 6,
                                 "7" = 7,
                                 "8" = 8,
                                 "9" = 9,
                                 "10" = 10,
                                 "11" = 11,
                                 "12" = 12),
                  selected = 6),
      #selectInput("difference1", label = "Long-term bond",
      #            choices = colnames(data.agg),
      #            selected = "SVENY10"),
      #selectInput("difference2", label = "Short-term bond",
      #            choices = colnames(data.agg),
      #            selected = "SVENY02"),
      selectizeInput("difference", label="Bond Spread",
                     options = list(maxItems = 2),
                     choices = colnames(data.agg),
                     selected = c("SVENY10","SVENY02"),
                     multiple = TRUE),
      checkboxInput("showgrid", label = "Show Grid", value = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Bond Yields", dygraphOutput("dygraph")),
        tabPanel(title = "2006-2007 Forecast", highchartOutput("histForecast")),
        tabPanel(title = "Forecast", highchartOutput("Forecast"))
        ) #close tabsetpanel
    
    ) #close mainpanel
    
  ) #close sidebar layout
  
) #close ui 

# Server

server<-function(input, output) {
  
  bondInput <- reactive({ 
    data.xts1[, input$bond]
  })

  output$dygraph <- renderDygraph({
    dygraph(bondInput(),main = "Cumulative Returns",ylab = "Yield(%)",xlab = "time") %>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.2,
                  highlightSeriesOpts = list(strokeWidth = 1.5),
                  hideOnMouseOut = FALSE) %>%
      dySeries(colnames(input$bond), label = colnames(input$bond)) %>%
      dyLegend(width = 400) %>%
      dyOptions(drawGrid = input$showgrid) %>%
      dyRangeSelector(height = 25) %>%
      dyShading(from = "1990-07-01", to = "1991-03-01", color = "rgba(10, 89, 4, 0.2)", axis = "x") %>%
      dyShading(from = "2001-03-01", to = "2001-12-01", color = "#FFE6E6", axis = "x") %>%
      dyShading(from = "2007-12-01", to = "2009-06-01", color = "rgba(200, 200, 230, 0.4)", axis = "x")
  })
  
  output$Forecast <- renderHighchart({
    
    spread <- data.agg[,input$difference[1]]-data.agg[,input$difference[2]]
    spread.ts <- as.ts(spread)
    spread.ts1 <- window(spread.ts, start = 1990, end=2019)
    
    highchart() %>%
      hc_add_series(spread.ts1,name="Spread") %>%
      hc_add_series(
        arima(spread.ts1, order=c(0,2,1)) %>% 
        forecast::forecast(h=input$periods)) %>%
      hc_title(text = "Treasury Historical Yields and Forecast") %>% 
      hc_yAxis(title = list(text = "monthly yields"),
               labels = list(format = "{value}%"),
               opposite = FALSE) %>%
      hc_xAxis(type = 'datetime', dateTimeLabelFormats = list(day = '%Y')) %>%
      hc_add_theme(hc_theme_flat()) %>% 
      hc_navigator(enabled = TRUE)
  })
  
  output$histForecast <- renderHighchart({
    
    spread <- data.agg[,input$difference[1]]-data.agg[,input$difference[2]]
    spread.ts <- as.ts(spread)
    spread.ts1 <- window(spread.ts, start=c(1990,1), end=c(2006,6))
    
    highchart() %>%
      hc_add_series(spread.ts1,name="Spread") %>%
      hc_add_series(
        arima(spread.ts1, order=c(0,2,1)) %>% 
          forecast::forecast(h=input$periods)) %>%
      hc_title(text = "Treasury Historical Yields and Forecast") %>% 
      hc_yAxis(title = list(text = "monthly yields"),
               labels = list(format = "{value}%"),
               opposite = FALSE) %>%
      hc_xAxis(type = 'datetime', dateTimeLabelFormats = list(day = '%Y')) %>%
      hc_add_theme(hc_theme_flat()) %>% 
      hc_navigator(enabled = TRUE)
  })
  
}

# Run App 
shinyApp(ui = ui, server = server)

