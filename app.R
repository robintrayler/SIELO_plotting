## Visualization tool for Field to Lab
## Robin B. Trayler May 6, 2019
## Load required libraries-----------------------------------------------------
library(shiny)
library(plotly)
library(lubridate)
library(tidyverse)
library(shinythemes)
library(shinyWidgets)
library(bslib)
library(DT)

## Set up UI-------------------------------------------------------------------
ui <- fluidPage(
  ## Set theme-----------------------------------------------------------------
  theme = bslib::bs_theme(primary   = '#0F2D52',
                          secondary = '#DAA900',
                          success = '#6ba43a',
                          danger = '#f18a00',
                          info = '#99d9d9',
                          font_scale = 1,
                          bootswatch = "minty",
                          base_font = "Helvetica",
                          heading_font = "Helvetica",
                          `enable-gradients` = TRUE,
                          `enable-shadows` = TRUE,
                          spacer = "0.5rem"),
  ## Set up header-------------------------------------------------------------
  headerPanel(title = 'Stable Isotope Ecosystem Lab of UC Merced: Plotting'), # set page title
  
  ## Set up sidebar------------------------------------------------------------
  sidebarLayout(
    sidebarPanel(width = 3,
                 markdown('## Instructions
                        Please upload a `.csv` file that follows [tidy data](https://r4ds.hadley.nz/data-tidy.html) conventions.
                        '),
                 fileInput(inputId = 'file_data',
                           label = 'upload a .csv',
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 markdown('#### Plot Type'),
                 selectInput(inputId = 'plot_type',
                             label = '',
                             choices = c('1', 
                                         '2',
                                         '3')),
                 markdown('#### Plot Variables'),
                 fluidRow(
                   column(width = 6,
                          selectInput(inputId = 'x_val', # drop down for plotting
                                      label = 'X axis', 
                                      choices = c('1','2','3'))),
                   column(width = 6,
                          selectInput(inputId = 'y_val', # drop down for plotting
                                      label = 'Y axis', 
                                      choices = c('1','2','3')))),
                 markdown('#### Plot Styling'),
                 fluidRow(
                   column(width = 6,
                          selectInput(inputId = 'group',
                                      label = 'Group',
                                      choices = c('1', '2', '3'))),
                   column(width = 6,
                          selectInput(inputId = 'color', # drop down for plotting
                                      label = 'Color', 
                                      choices = c('1','2','3'))),
                   column(width = 6,
                          selectInput(inputId = 'shape', # drop down for plotting
                                      label = 'Shape', 
                                      choices = c('1','2','3')))),
                 markdown('#### Subplots & Summary'),
                 fluidRow(
                   column(width = 6,
                          shiny::selectInput(inputId = 'split',
                                             label = 'Subplot',
                                             choices = c('1', '2', '3'))),
                   column(width = 6,
                          shiny::selectInput(inputId = 'sum_type',
                                             label = 'Summarize',
                                             choices = c('none', 'lm', 'loess', 'ellipses'))))),
    
    ## Set main panel------------------------------------------------------------
    mainPanel( # set up a tabbed panel 
      tabsetPanel(
        type = 'pills',
        tabPanel('Plot', 
                 plotlyOutput('plot',
                              height = '650px', 
                              width = '100%'),
                 downloadButton(outputId = 'download')), # tab for plot
        tabPanel('Data', dataTableOutput('table'))
      )
    )
  )
)

## Set up Server---------------------------------------------------------------
server <- shinyServer(function(input, output, session) {
  # bs_themer()
  
  ## load data ----------------------------------------------------------------
  data <- reactive({
    if(is.null(input$file_data)) {return(NULL)}
    # read the uploaded data
    data <- read.csv(input$file_data$datapath)
    return(data)
  })
  
  # get a list of column headers for the menues
  options <- reactive({lapply(X = data(), FUN  = class) |> unlist()})
  
  # parse the headers into diferent classes
  characters <- reactive({names(data())[which(options() == 'character')]})
  numerics   <- reactive({names(data())[which(options() == 'numeric')]})
  factors    <- reactive({names(data())[which(options() == 'factor')]})
  integers   <- reactive({names(options())[which(options() == 'integer')]})
  
  ## Update menu options-------------------------------------------------------
  ## update menu options once data is loaded
  observeEvent(data(),
               updateSelectInput(session,
                                 inputId =  "plot_type",
                                 choices = c('scatter',
                                             'line',
                                             'scatter & line',
                                             'boxplot',
                                             'violin',
                                             'histogram',
                                             'density'),
                                 selected =  'scatter'))
  observeEvent(data(),
               updateSelectInput(session,
                                 inputId =  "group",
                                 choices = c('none', 
                                             characters(), 
                                             factors())))
  
  # update menus ------------------------------------------
  observeEvent(data(), {
    observeEvent(input$plot_type,
                 if(input$plot_type == 'scatter' || 
                    input$plot_type == 'line' || 
                    input$plot_type == 'scatter & line') {
                   updateSelectInput(session,
                                     inputId =  "x_val",
                                     choices = c(numerics(), 
                                                 characters(), 
                                                 integers()))
                   
                   # update y value choices
                   updateSelectInput(session,
                                     inputId =  "y_val",
                                     choices = c(numerics(), 
                                                 characters(), 
                                                 integers()))
                   
                 } else if(input$plot_type == 'histogram' || input$plot_type == 'density') {
                   updateSelectInput(session,
                                     inputId =  "x_val",
                                     choices = c(numerics(),
                                                 integers()))
                   ## update y value menu
                   updateSelectInput(session,
                                     inputId =  "y_val",
                                     choices = NA)
                 } else if(input$plot_type == 'boxplot' || input$plot_type == 'violin'){
                   updateSelectInput(session,
                                     inputId =  "x_val",
                                     choices = c(characters(), 
                                                 factors()))
                   ## update y value menu
                   updateSelectInput(session,
                                     inputId =  "y_val",
                                     choices = c(numerics(), 
                                                 integers()))
                 })
  })
  
  # update color menu
  observeEvent(data(), {
    updateSelectInput(session,
                      inputId =  "color",
                      choices = c('none', 
                                  characters(), 
                                  factors()))})
  
  # update shape menu
  observeEvent(data(), {
    updateSelectInput(session,
                      inputId =  "shape",
                      choices = c('none', 
                                  characters(), 
                                  factors()))})
  
  # update facet menu
  observeEvent(data(), {
    updateSelectInput(session,
                      inputId =  "split",
                      choices = c('none', 
                                  characters(), 
                                  factors()))})
  
  # ## Plot selected variables---------------------------------------------------
  # assemble a ggplot argument 
  string <- reactive({ 
    string<- paste0(
      'data () |> ggplot(mapping = aes(', 
      if(input$plot_type == 'scatter' || input$plot_type == 'line' || input$plot_type == 'scatter & line') {
        'x = .data[[input$x_val]], y = .data[[input$y_val]]'
      } else if(input$plot_type == 'boxplot' || input$plot_type == 'violin') {
        'x = .data[[input$x_val]] , y = .data[[input$y_val]]'
      } else if(input$plot_type == 'density' || input$plot_type == 'histogram') {
        'x = .data[[input$x_val]]'
      },
      
      if(input$color != 'none') {
        ', color = .data[[input$color]], fill = .data[[input$color]]'
      }, 
      
      if(input$group != 'none') {
        ', group = .data[[input$group]]'
      },
      
      if(input$shape != 'none') {
        ', shape = .data[[input$shape]]'
      }
      ,')' # close aes()
      ,')', # close ggplot()
      
      # add geoms
      if(input$plot_type == 'scatter') {
        '+ geom_point(size = 3, alpha = 0.65)'
      } else if(input$plot_type == 'line') {
        '+ geom_line(alpha = 0.75)'
      } else if(input$plot_type == 'scatter & line') {
        '+ geom_point(size = 3, alpha = 0.65) + geom_line(alpha = 0.75)'
      } else if(input$plot_type == 'boxplot') {
        '+ geom_boxplot(alpha = 0.75)' 
      } else if(input$plot_type == 'violin') {
        '+ geom_violin(alpha = 0.75)' 
      } else if(input$plot_type == 'density') {
        '+ geom_density(alpha = 0.75)'
      } else if(input$plot_type == 'histogram') {
        '+ geom_histogram(alpha = 0.75)'
      },
      # add summary 
      if(input$sum_type != 'none') {
        if(input$sum_type == 'lm') {
          '+ geom_smooth(method = "lm")'
        } else if(input$sum_type == 'loess') {
          '+ geom_smooth(method = "loess")'
        } else if(input$sum_type == "ellipses") {
          '+ stat_ellipse()'
        }
      },
      
      # add facets
      if(input$split != 'none') {
        '+ facet_wrap(~ .data[[input$split]])'
      },
      
      # add theme
      '+ theme_classic() + theme(legend.position = "top", axis.text = element_text(size = 12), axis.title = element_text(size = 12))'
    )
  })
  
  ## outputs ------------------------------------------------------------------
  # render the plot
  output$plot <- renderPlotly({
    if(is.null(data())) {return(NULL)}
    p <- parse(text = string()) |> eval() |> ggplotly()
    p})
  
  # make a plot for the download button
  download_plot <- reactive({
    if(is.null(data())) {return(NULL)}
    p <- parse(text = string()) |> eval()})
  
  # add a table to the second tab
  output$table <- renderDataTable(data())
  
  # download a pdf of the plot
  output$download <- downloadHandler(filename = 'plot.pdf',
                                     content = function(file) {
                                       ggsave(file, plot = download_plot(), 
                                              width = 6,
                                              height = 6, 
                                              units = "in", 
                                              device = "pdf")})
  
  ##---------------------------------------------------------------------------
}
)
shinyApp(ui = ui, server = server)



