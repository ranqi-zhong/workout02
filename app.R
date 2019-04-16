library(rsconnect)
library(shiny)
library(ggplot2)
library(tidyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Saving-Investing Modalities"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
     column(3,
         sliderInput("iamount", "Initial Amount:",
                     min = 0, max = 100000,
                     value = 1000, step = 500,
                     pre = "$", sep = ",",
                     animate = TRUE),
         sliderInput("acontrib", "Annual Contribution:",
                     min = 0, max = 50000,
                     value = 2000, step = 500,
                     pre = "$", sep = ",",
                     animate = TRUE)
         ),
     
     column(4, 
         sliderInput("rrate", "Return Rate(in %):",
                     min = 0, max = 20,
                     value = 5, step = 0.1),
         sliderInput("grate", "Growth Rate(in %):",
                     min = 0, max = 20,
                     value = 2, step = 0.1)
         ),
     column(5,
         sliderInput("year", "Year:",
                     min = 0, max = 50,
                     value = 20, step = 1),
         br(),
         selectInput("facet","Facet?",
                     choices = c("No","Yes"))
         )
   ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h4("Timelines"),
        plotOutput("plot"),
        h4("Balances"),
        DT::dataTableOutput("table")
         
      )
   )
future_value <- function(amount, rate, years){
  pv = amount
  factor = (1 + rate) ^ years
  fv = pv * factor
  return(fv)
}

annuity <- function(contrib, rate, years){
  c = contrib
  fac = (1 + rate) ^ years
  factor = (fac - 1)/(rate)
  fva = c * factor
  return(fva)
}

growing_annuity <- function(contrib, rate, growth, years){
  c = contrib
  fac = ((1 + rate) ^ years - (1 + growth) ^ years)
  factor = fac/(rate - growth)
  fvga = c * factor
  return(fvga)
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  modalities <- reactive({
    n = input$year
    x = 0
    a = c(rep(NA,input$year))
    b = c(rep(NA,input$year))
    c = c(rep(NA,input$year))
    d = c(rep(NA,input$year))
    for (x in 0:n) {
      a[x] = x
      b[x] = future_value(amount = input$iamount, rate = input$rrate/100, years = x)
      c[x] = b[x] + annuity(contrib = input$acontrib, rate = input$rrate/100, years = x)
      d[x] = b[x] + growing_annuity(contrib = input$acontrib, rate = input$rrate/100, growth = input$grate/100, years = x)
    }
  modalities1 = data.frame(year = c(0,a), no_contrib = c(input$iamount,b), fixed_contrib = c(input$iamount,c), growing_contrib = c(input$iamount,d))
  options(digits=2)
    return(modalities1)
  })
  
  output$plot <- renderPlot(
    if(input$facet == "No"){
    ggplot(modalities())+
      geom_line(aes(x = modalities()$year, y = modalities()$no_contrib, col = "yellow"))+
      geom_line(aes(x = modalities()$year, y = modalities()$fixed_contrib, col = "red"))+
      geom_line(aes(x = modalities()$year, y = modalities()$growing_contrib,col = "blue"))+
      scale_colour_manual(values=c("yellow" ,"red","blue"), label = c("growing_contrib", "fixed_contrib", "no_contrib"))}
    else if(input$facet == "Yes"){
      row = cbind(c(input$iamount,b,input$iamount,c,input$iamount,d))
      year = cbind(c(0,a,0,a,0,a))
      type <- c(rep("no_contrib",21),rep("fixed_contrib",21),rep("growing_contrib",21))
      type <- as.factor(type)
      modalities2 <- data.frame(type,year,row)
      ggplot(modalities2)+
        geom_area(aes(x = modalities2$year,y = modalities2$row,color = factor(type),fill = factor(type)))+
        facet_grid(type)
      }
  )
  
  
  output$table <- DT::renderDataTable(DT::datatable({
    modalities()
  }))
  
}

# Run the application 
shinyApp(ui = ui, server = server)
