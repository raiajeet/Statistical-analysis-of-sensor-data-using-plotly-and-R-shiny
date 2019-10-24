terasida1=read.csv("C:\\Users\\AJIT\\Documents\\EDA\\teras-IDA-1_20171201-1322541.csv",header=TRUE, sep=",") 
terasida2=read.csv("C:\\Users\\AJIT\\Documents\\EDA\\teras-IDA-2_20171201-1340111.csv",header=TRUE, sep=",") 
terasidb1=read.csv("C:\\Users\\AJIT\\Documents\\EDA\\teras-IDB-1_20171201-1345521.csv",header=TRUE, sep=",") 
terasidb2=read.csv("C:\\Users\\AJIT\\Documents\\EDA\\teras-IDB-2_20171201-1407201.csv",header=TRUE, sep=",") 

terascep1=read.csv("C:\\Users\\AJIT\\Documents\\EDA\\teras-cep-1_20171201-0955021.csv",header=TRUE, sep=",") 
terascep2=read.csv("C:\\Users\\AJIT\\Documents\\EDA\\teras-cep-2_20171201-1024441.csv",header=TRUE, sep=",") 
terascep3=read.csv("C:\\Users\\AJIT\\Documents\\EDA\\teras-cep-3_20171201-1049371.csv",header=TRUE, sep=",") 

terastub1=read.csv("C:\\Users\\AJIT\\Documents\\EDA\\teras-TURB-7_20171129-121430.csv",header=TRUE, sep=",") 
terastub2=read.csv("C:\\Users\\AJIT\\Documents\\EDA\\teras-TURB-8_20171129-122236.csv",header=TRUE, sep=",") 
terastub3=read.csv("C:\\Users\\AJIT\\Documents\\EDA\\teras-turb-9_20171129-123051.csv",header=TRUE, sep=",") 
terastub4=read.csv("C:\\Users\\AJIT\\Documents\\EDA\\teras-turb-9_20180307-190636.csv",header=TRUE, sep=",") 


terasida1=subset(terasida1,select=-c(X,DATE.TIME))
terasida2=subset(terasida2,select=-c(X,DATE.TIME))
terasidb1=subset(terasidb1,select=-c(X,DATE.TIME))
terasidb2=subset(terasidb2,select=-c(X,DATE.TIME))
IDF=cbind(terasida1,terasida2,terasidb1,terasidb2)

terascep1=subset(terascep1,select=-c(X,DATE.TIME))
terascep2=subset(terascep2,select=-c(X,DATE.TIME))
terascep3=subset(terascep3,select=-c(X,DATE.TIME))
CEP=cbind(terascep1,terascep2,terascep3)

terastub1=subset(terastub1,select=-c(X,Date.Time))
terastub2=subset(terastub2,select=-c(X,Date.Time))
terastub3=subset(terastub3,select=-c(X,Date.Time))
terastub4=subset(terastub4,select=-c(X,Date.Time))
TUR=cbind(terastub1,terastub2,terastub3)


mean=colMeans(IDF,na.rm=T)
max=apply(IDF,2,max,na.rm=T)
min=apply(IDF,2,min,na.rm=T)
p1 <- rbind(min,mean,max)
p1<- as.data.frame(t(p1))
p1$f=colnames(IDF)

mean=colMeans(CEP,na.rm=T)
max=apply(CEP,2,max,na.rm=T)
min=apply(CEP,2,min,na.rm=T)
p2 <- rbind(min,mean,max)
p2<- as.data.frame(t(p2))
p2$f=colnames(CEP)

mean=colMeans(TUR,na.rm=T)
max=apply(TUR,2,max,na.rm=T)
min=apply(TUR,2,min,na.rm=T)
p3 <- rbind(min,mean,max)
p3<- as.data.frame(t(p3))
p3$f=colnames(TUR)

mean=colMeans(terastub4,na.rm=TRUE)
max=apply(terastub4,2,max,na.rm=TRUE)
min=apply(terastub4,2,min,na.rm=TRUE)
q<-rbind(min,mean,max)
q<- as.data.frame(t(q))
q$f=colnames(terastub4)


library(shiny)
library(plotly)

ui <- fluidPage(
             fluidRow(
                      column(6,plotlyOutput('plot')),
                      column(6,plotlyOutput('plot1')),
                      column(6,plotlyOutput('plot2')),
                      column(6,plotlyOutput('plot3'))
                      ))
server <- function(input, output){
  output$plot <- renderPlotly({
    y <- list(
      title = "")
  plot=plot_ly(p1, x = ~f, y = ~min, type = 'bar', name = 'Min') %>%
    layout(title = "IDF") %>%
    add_lines(y = ~mean, name = 'Mean', type = 'scatter',mode = 'lines',line = list(color ="brown")) %>%
    add_trace(y = ~max, name = 'Max')%>%
    layout(yaxis =y,autosize = F, width = 650, height = 350,showlegend = FALSE)
  })
  
  output$plot1 <- renderPlotly({
    y <- list( title = "")
     plot_ly(p2, x = ~f, y = ~min, type = 'bar', name = 'Min') %>%
      layout(
        title = "CEP") %>%
       add_lines(y = ~mean, name = 'Mean', type = 'scatter',mode = 'lines',line = list(color ="brown")) %>%
       add_trace(y = ~max, name = 'Max')%>%
       layout(yaxis =y,autosize = F, width = 650, height = 350)
  })
  output$plot2 <- renderPlotly({
    y <- list(
      title = "")
    plot_ly(p3, x = ~f, y = ~min, type = 'bar', name = 'Min') %>%
      layout(
        title = "TUR1") %>%
      add_lines(y = ~mean, name = 'Mean', type = 'scatter',mode = 'lines',line = list(color ="brown")) %>%
      add_trace(y = ~max, name = 'Max') %>%
      layout(yaxis =y,autosize = F, width = 650, height = 250,showlegend = FALSE)
  })
  output$plot3 <- renderPlotly({
    y <- list(
      title = "")
    plot_ly(q, x = ~f, y = ~min, type = 'bar', name = 'Min') %>%
      layout(title = "TUR2") %>%
      add_lines(y = ~mean, name = 'Mean', type = 'scatter',mode = 'lines',line = list(color ="brown")) %>%
      add_trace(y = ~max, name = 'Max')%>%
      layout(yaxis =y,autosize = F, width = 650, height = 250,showlegend = FALSE)
  })
}
shinyApp(ui, server)