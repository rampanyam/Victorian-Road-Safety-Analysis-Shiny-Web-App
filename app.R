#### Raghuram Panyam 
#### 30302382 
#### Tutor : Amir Malekianvar 
### Tutorial Time and Day : Monday 6 - 8 PM 


# Please uncomment to install the packages:
# install.packages("shiny")
# install.packages("plotly")
# install.packages("shinythemes")
# install.packages("shinydashboard")
# install.packages("dpllyr")
# install.packages("shinycssloaders")
# install.packages("collapsibleTree")
# install.packages("reshape2")
# install.packages("ggthemes")
# install.packages("shinywidgets")[]


### The libraries for the project : 
library(shiny) # Shiny dashboard
library(plotly) # Interactive web visualisations with plotly
library(shinythemes) # theme for the nav bar page
library(shinydashboard) # package for using dashboard objects
library(shinycssloaders) # package for loading spinners
library(dplyr) # Data Frame Operations
library(ggplot2) # Pakage for Plotting 
library(collapsibleTree) # Dendrogram 
library(reshape2) # Melting the data frame 
library(ggthemes) # package for themes 
library(shinyWidgets) # Package for Shinywidgets. 



## Reading the DataFrrames
accident_data_frame <- read.csv('Accidents_Final.csv')
person_vehicle <- read.csv('Person_Vehicle.csv')




############### ARRANGING DATA FOR TAB 1 ################################################################

############ Data for Plot 1 : 

### DataFrame for Accidents per Region ########################################################
### #Basic data frame operations are performed to visualise the grouped bar plot and the simple bar plot : 

region_data_frame <- accident_data_frame[,c('REGION_NAME','SERIOUSINJURY','FATALITY')]
region_accident <- data.frame(summary(region_data_frame$REGION_NAME))
region_accident <- cbind(region_accident,rownames(region_accident)) 
colnames(region_accident) <- c("Accidents","Region")
rownames(region_accident) <- NULL
head(region_accident)






############# DataFrame for Plot  1 On Click ##########################################
# DF operations for On click table 

alcohol_data_frame <- accident_data_frame[,c('REGION_NAME','SERIOUSINJURY','FATALITY','ALCOHOL_RELATED')]
alcohol_data_frame$ALCOHOL_RELATED <- ifelse(alcohol_data_frame$ALCOHOL_RELATED == "Yes",1,0)
alcohol_data_frame <- aggregate(alcohol_data_frame$ALCOHOL_RELATED, by = list(alcohol_data_frame$REGION_NAME),FUN=sum)
colnames(alcohol_data_frame) <- c("Region","Alcoholic_accidents")
unliscened_data_frame <- accident_data_frame[,c('REGION_NAME','SERIOUSINJURY','FATALITY','UNLICENCSED')]
unliscened_data_frame <- aggregate(unliscened_data_frame$UNLICENCSED, by = list(unliscened_data_frame$REGION_NAME),FUN=sum)
colnames(unliscened_data_frame) <- c("Region","Unlicensed_accidents")
regions_stats <- data.frame(alcohol_data_frame, "unlicensed_accidents"  = unliscened_data_frame$Unlicensed_accidents)


########### Data Frame for INJURIES AND FATALITIES PER REGION #######################################
# DF Operations for Comparision analysis : 


region_data_frame <- accident_data_frame[,c('REGION_NAME','SERIOUSINJURY','FATALITY')]
region_analysis <- melt(region_data_frame, id.vars='REGION_NAME')
fatality_df <- region_analysis[region_analysis$variable == "FATALITY",]
fatality_df <- aggregate(fatality_df$value,by = list(fatality_df$REGION_NAME),FUN=sum)
colnames(fatality_df) <- c("Region","fatality")
injury_df <- region_analysis[region_analysis$variable == "SERIOUSINJURY",]
injury_df <- aggregate(injury_df$value,by = list(injury_df$REGION_NAME),FUN=sum)
colnames(injury_df) <- c("Region","injury")
plot_2_df<- data.frame(fatality_df,"injury" = injury_df$injury)
head(plot_2_df)


### Data for plot 2 : 
############### DataFrame for pLot 2 tab1 ###############################

### Data for Comparision analysis on click table 

sub_acc <- accident_data_frame[,c("Year","STAT_DIV_NAME","SERIOUSINJURY","FATALITY")]
country <- sub_acc[sub_acc$STAT_DIV_NAME == "Country",]
injurycountry <- aggregate(country$SERIOUSINJURY,by=list(country$Year),FUN=sum)
colnames(injurycountry) <- c("Year","Injury")
fatalitycountry <- aggregate(country$FATALITY,by=list(country$Year),FUN=sum)
colnames(fatalitycountry) <-c("Year","Fatality")


sub_acc <- accident_data_frame[,c("Year","STAT_DIV_NAME","SERIOUSINJURY","FATALITY")]
Metro <- sub_acc[sub_acc$STAT_DIV_NAME == "Metro",]
injuryMetro <- aggregate(Metro$SERIOUSINJURY,by=list(Metro$Year),FUN=sum)
colnames(injuryMetro) <- c("Year","Injury")
fatalityMetro <- aggregate(Metro$FATALITY,by=list(Metro$Year),FUN=sum)
colnames(fatalityMetro) <-c("Year","Fatality")

fatalitydf2 <- data.frame("Year" = fatalityMetro$Year,"Metro" = fatalityMetro$Fatality,"country" = fatalitycountry$Fatality)
injurydf2 <- data.frame("Year" = fatalityMetro$Year,"Metro" = injuryMetro$Injury,"country" = injurycountry$Injury)






############## Function for TAB 1 PLOT1 #####################################################
### This function gives a plot based on the radio button click


########################## CODE FOR PLOT 1 ###########################################################

plotType <- function(type) {
  
  marker_style <- list(line = list(width = 1,color = 'black'))
  plot_1_tab1 <- plot_ly(region_accident, source = "event1plot1", x = ~Region, y = ~ Accidents, type = 'bar',text = ~Region,color = ~ as.factor(Region),colors = "Dark2",
                  width = 1.5,marker=marker_style,
                  hovertemplate = paste(
                    
                    "%{yaxis.title.text}: %{y:.0f}<br>",
                    "%{xaxis.title.text}: %{x}<br>",
                    "<extra></extra>"
                  ))  
  
plot_1_tab1 <- plot_1_tab1 %>% layout(title = "REGION ANALYSIS",legend = list(orientation = 'h', y = -0.3))




########### CODE FOR PLOT 2 #######################################################
plot_2_tab1 <- plot_ly(plot_2_df, source = "event1plot1",x = ~Region, y = ~injury, type = 'bar', name = 'Injury')
plot_2_tab1 <- plot_2_tab1 %>% add_trace(y = ~fatality, name = 'fatality')
plot_2_tab1 <- plot_2_tab1 %>% layout(yaxis = list(title = 'Count'), barmode = 'group')


################## Code to Merge INput with the Plots ####################################################
  
  
  switch(type,
         A = plot_2_tab1 ,
         B = plot_1_tab1)
}







####### Textoutput Function for plot 1 ################ 

tr <- function(name) return(paste("Now you are viewing ",name," per  Region "))





############# Function for Tab 1  PLOT-2 #######################################################################################################
### This function gives a plot based on the radio button click


plotType_2 <- function(type) {
  
  
### Code for Option1:  
  plt2_tb2 <- plot_ly(fatalitydf2,source = "event1plot2",x = ~Year, y = ~Metro, type = 'bar', name = 'Metro',hovertemplate = paste(
    
    "%{yaxis.title.text}: %{y:.0f}<br>",
    "%{xaxis.title.text}: %{x}<br>",
    "<extra></extra>"
  ))
  plt2_tb2 <- plt2_tb2 %>% add_trace(y = ~ country, name = 'country')
  plt2_tb2 <- plt2_tb2 %>% layout(yaxis = list(title = 'Count'), barmode = 'group',title = "Metro vs Country Fatality")
  
  
#### Code for Option2:
  
  
plt21_tb2 <- plot_ly(injurydf2,source ="event1plot2", x = ~Year, y = ~Metro, type = 'bar', name = 'Metro',hovertemplate = paste(
    
    "%{yaxis.title.text}: %{y:.0f}<br>",
    "%{xaxis.title.text}: %{x}<br>",
    "<extra></extra>"
  ))
  plt21_tb2 <- plt21_tb2 %>% add_trace(y = ~ country, name = 'country')
  plt21_tb2 <- plt21_tb2 %>% layout(yaxis = list(title = 'Count'),title = "Metro Vs Country Injuries", barmode = 'group')
  
  #### COde to bind the options : 
  switch(type,
         C = plt21_tb2 ,
         D = plt2_tb2 )
}

############ If you click on plot2 tab 1 this table will pop up !! ###############
unlicenseddftab1 <- data.frame("Year"  = accident_data_frame$Year,  "Alcohol" = accident_data_frame$ALCOHOL_RELATED,"unlicensed" = accident_data_frame$UNLICENCSED)
unlicenseddftab1$unlicensed <- ifelse(unlicenseddftab1$Alcohol == "Yes",1,0)
tab1plot2subdf <- aggregate(.~Year,FUN=sum,unlicenseddftab1)


################## ARRANGING DATA FOR SPEED ZONE + ACCIDENT TYPE ANALYSIS #######################################################
accident_data_frame$SEVERITY <- as.character(accident_data_frame$SEVERITY)
abs <- accident_data_frame[accident_data_frame$SEVERITY == "Fatal accident",]
question1 <- data.frame(Accident_type = abs$ACCIDENT_TYPE,Speed_Zone = abs$SPEED_ZONE)
ddf<- aggregate(question1,by=list(question1$Accident_type,question1$Speed_Zone),FUN = length)
question1final <- data.frame(Group = paste(ddf$Group.1,'-',ddf$Group.2),Count = ddf$Accident_type)
finalquestion <- question1final[order(-question1final$Count),]
rownames(finalquestion) <- NULL
finaldata_1 <- as.data.frame(head(finalquestion,10),rownames = T)
colnames(ddf) <- c('group','subgroup','value','value2')
ddf <- ddf[,names(ddf) != 'value2']


######## Arranging Data for Tab 2 ###################################################################
#### The below code is for the arrangement for Stacked Bar Chart:

accident_data_frame_duplicate <- accident_data_frame
l <- levels(accident_data_frame$SPEED_ZONE)
a <- l[!levels(accident_data_frame$SPEED_ZONE) %in% c('Camping grounds or off road','Not known','Other speed limit')]
accident_data_frame_duplicate$SPEED_ZONE <- as.character(accident_data_frame_duplicate$SPEED_ZONE)
accident_data_frame_2 <- accident_data_frame_duplicate[accident_data_frame_duplicate$SPEED_ZONE %in% a,]
adf <- accident_data_frame_2[,c('SPEED_ZONE','SERIOUSINJURY','FATALITY')]
adf2 <- aggregate(.~SPEED_ZONE,adf,FUN=sum)
rownames(adf2) <- NULL




#### Arranging Data FOr DOnut CHart :
### This function gives a plot based on the radio button click

donutdata <- accident_data_frame[!(accident_data_frame$LIGHT_CONDITION) %in% c('Unk.','Dark Street lights unknown'),]
donutdata <- data.frame(a= donutdata$LIGHT_CONDITION)
donutdata <- donutdata %>% 
  group_by(a) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(per=`n`/sum(`n`)) %>% 
  arrange(desc(a))
colnames(donutdata) <- c("category","count","fraction")

donutdata2 <- accident_data_frame[!(accident_data_frame$LIGHT_CONDITION) %in% c('Unk.','Dark Street lights unknown'),]
donuttdata21 <- data.frame("Light_condition" = donutdata2$LIGHT_CONDITION,"Year" = donutdata2$Year)





################# FUnction for Tab-2 #############################################
### This function gives a plot based on the Select Input / Drop Down Menu Click !! 
plotType_tab2 <- function(type){
  
  speedplot <- plot_ly(adf2, x = ~SPEED_ZONE, y = ~FATALITY, type = 'bar', name = 'FATALITY',source = "subline",key= ~SPEED_ZONE,hovertemplate = paste(
    "%{yaxis.title.text}: %{y:.0f}<br>",
    "%{xaxis.title.text}: %{x}<br>",
    "<extra></extra>"
  ),colors = c("blue","green"))
  speedplot <- speedplot %>% add_trace(y = ~ SERIOUSINJURY, name = 'SERIOUSINJURY')
  speedplot <- speedplot %>% layout(yaxis = list(title = 'Count'),title = "SPEEDZONE ANALYSIS", barmode = 'stack')
  
  
  geometry_1  <- accident_data_frame[,c('ROAD_GEOMETRY','SERIOUSINJURY','FATALITY')]
  geometry_11 <- aggregate(.~ROAD_GEOMETRY,geometry_1,FUN=sum)
  geometryplot <- plot_ly(geometry_11, x = ~ROAD_GEOMETRY, y = ~FATALITY, type = 'bar', name = 'FATALITY',source = "subline",key= ~ROAD_GEOMETRY,hovertemplate = paste(
    
    "%{yaxis.title.text}: %{y:.0f}<br>",
    "%{xaxis.title.text}: %{x}<br>",
    "<extra></extra>"
  ),colors = c("blue","green"))
  geometryplot <- geometryplot %>% add_trace(y = ~ SERIOUSINJURY, name = 'SERIOUSINJURY')
  geometryplot <- geometryplot %>% layout(yaxis = list(title = 'Count'),title = "ROADGEOMETRY ANALYSIS", barmode = 'stack')
  ### Binding the polots
  switch(type,
         speedzone = speedplot,
         geometry = geometryplot )
  
  
}




######### arranging speed data for Tab2 Sub Scatter Plot ########### : 

speeddf <- accident_data_frame
speeddf$ALCOHOL_RELATED <- ifelse(speeddf$ALCOHOL_RELATED == "Yes",1,0)
### ROad geometry subbplot: 
subgeom <- accident_data_frame[,c('Year','ROAD_GEOMETRY','ALCOHOL_RELATED','UNLICENCSED')]


### Function for Subbplot #####################################################################

#### This Function returns a line plot if you click on plot   1 in Tab2

plotType_subplot <- function(keyval){
  if(keyval %in%  as.vector(levels(speeddf$SPEED_ZONE))){
    ### Arranging Data for Speed Analysis:
    speeddf <- speeddf[speeddf$SPEED_ZONE == keyval,]
    speeddf2 <- data.frame("unlicensed" = speeddf$UNLICENCSED, "alcohol" = speeddf$ALCOHOL_RELATED,"Year" = speeddf$Year)
    speeddf2 <- aggregate(.~Year,speeddf2,FUN=sum)
    ### On CLick Scatter plot : 
    figspeed <- plot_ly(speeddf2, x = ~Year, y = ~unlicensed, name = 'Unlicensed Accidents', type = 'scatter', mode = 'lines') 
    figspeed <- figspeed %>% add_trace(y = ~alcohol, name = 'Alcoholic accidents', mode = 'lines+markers') 
    figspeed <- figspeed %>%   layout(hovermode = "x unified",title = "Unlicensed vs Alcohlic For Speed Zone")
    figspeed
  } else if(keyval %in% as.vector(unique(subgeom$ROAD_GEOMETRY))){
    
    subgeom <- subgeom[subgeom$ROAD_GEOMETRY == keyval,]
    subgeom2 <- data.frame("unlicensed" = subgeom$UNLICENCSED, "alcohol" = subgeom$ALCOHOL_RELATED,"Year" = subgeom$Year)
    subgeom2 <- aggregate(.~Year,subgeom2,FUN=sum)
    figsubgeom <- plot_ly(subgeom2, x = ~Year, y = ~unlicensed, name = 'Unlicensed Accidents', type = 'scatter', mode = 'lines') 
    figsubgeom <- figsubgeom %>% add_trace(y = ~alcohol, name = 'Alcoholic accidents', mode = 'lines+markers') 
    figsubgeom <- figsubgeom %>%   layout(hovermode = "x unified",title = "Unlicensed vs Alcohlic ForRoad Geometry")
    figsubgeom
    
    
    
    
    
  }
  
}





######### Arranging Data FOr Tab 3 ################################################
#### Data for Plot 1 ######################

#### Accidents per road user type: 
accidentusertype <- data.frame(summary(person_vehicle$Road.User.Type.Desc))
usd <- data.frame(cbind(rownames(accidentusertype),accidentusertype))
rownames(usd) <- NULL
colnames(usd) <- c('roaduser','accident')
usd$roaduser <- as.character(usd$roaduser)
usd <- usd[usd$roaduser != 'Unknown',]


#### Accidents per User Age Group ################## 
### Arranging data for this inspection : 
accidentagetype <- data.frame(summary(person_vehicle$Age.Group))
column_2 <- row.names(accidentagetype)
row.names(accidentagetype) = NULL
accidentagetype <- cbind(accidentagetype,column_2)
colnames(accidentagetype) <- c('Accident.Count','Age.Group')
#optional filtering removing unkown: 
accidentagetype$Age.Group <- as.character(accidentagetype$Age.Group)
accidentagetype <- accidentagetype[accidentagetype$Age.Group != 'unknown',]
accidentagetype$Age.Group <- as.factor(accidentagetype$Age.Group)




##### Injuries and Fatalities Per year: 
yearanalysis <- data.frame("Year" = accident_data_frame$Year,"injury" = accident_data_frame$SERIOUSINJURY,"fatality" = accident_data_frame$FATALITY)
yearanalysisframe <- aggregate(.~Year,yearanalysis,FUN=sum)

########## FUnction FOr Injuries / Fatalities Per Year #################################

year_analysis_injury <- function(){
  
  fig_year_analysis1 <- plot_ly(yearanalysisframe, x = ~Year, y = ~injury, name = 'Injuries per Year', type = 'scatter', mode = 'lines',
                                hovertemplate = paste(
                                  "<b>{Detailed Analysis:}</b><br><br>",
                                  "%{yaxis.title.text}: %{y:.0f}<br>",
                                  "%{xaxis.title.text}: %{x}<br>",
                                  "<extra></extra>"
                                ))
  fig_year_analysis1 <- fig_year_analysis1 %>% layout(title = "Injuries Per Year")
  fig_year_analysis1
  
  
  
}


year_analysis_fatality <- function(){
  
  
  fig_year_analysis2 <- plot_ly(yearanalysisframe, x = ~Year, y = ~ fatality, name = 'Fatalities per Year', type = 'scatter', mode = 'lines',
                                hovertemplate = paste(
                                  "<b>{Detailed Analysis:}</b><br><br>",
                                  "%{yaxis.title.text}: %{y:.0f}<br>",
                                  "%{xaxis.title.text}: %{x}<br>",
                                  "<extra></extra>"
                                ))
  fig_year_analysis2 <- fig_year_analysis2 %>% layout(title = "Fatalities Per Year")
  fig_year_analysis2
  
  
  
  
  
}

year_analysis_both <- function(){
  
  
  fig_year_both <- plot_ly(yearanalysisframe, x = ~Year, y = ~injury, name = 'Injuries per Year', type = 'scatter', mode = 'lines',
                           hovertemplate = paste(
                             "<b>{Detailed Analysis:}</b><br><br>",
                             "%{yaxis.title.text}: %{y:.0f}<br>",
                             "%{xaxis.title.text}: %{x}<br>",
                             "<extra></extra>"
                           ))
  
  fig_year_both <- fig_year_both  %>% add_trace(y = ~fatality, name = 'Fatality per Year', mode = 'lines+markers')
  fig_year_both <- fig_year_both %>% layout(title = "Injuries and Fatalities Per Year",yaxis = list(title = "Count"))
  fig_year_both
  
}










##### Function for Barplot Tab 3 #######################################################
plotType_tab3_generalplot <- function(typevariable){
  

  ####  Plot 1 FOr Option1:
  ### This is the barplot code for accidents per road user  type: 
  marker_style <- list(line = list(width = 1,color = 'black'))
  generalplot1 <- plot_ly(usd, x = ~roaduser, y = ~accident, type = 'bar',text = ~roaduser,color = ~ as.factor(roaduser),colors = "Dark2",
                          width = 1.5,marker=marker_style,
                          hovertemplate = paste(
                            
                            "%{yaxis.title.text}: %{y:.0f}<br>",
                            "%{xaxis.title.text}: %{x}<br>",
                            "<extra></extra>"
                          )) 
  generalplot1 <- generalplot1 %>% layout(title = "ROADUSER ANALYSIS")
  generalplot1 <- generalplot1 %>% layout(legend = list(orientation = "h",xanchor = "center",  x = 0.5))            
  
  
  


  #### Barplot code for accidents per agegroup : 
  ########### Code for Barplot for accidents per Age Group : 
  
  marker_style <- list(line = list(width = 1,color = 'black'))
  generalplot2<- plot_ly(accidentagetype, x = ~Age.Group, y = ~Accident.Count, type = 'bar',text = ~Age.Group,color = ~ as.factor(Age.Group),colors =c("red","blue","green","yellow","pink","grey","orange","orangered","darkgreen","maroon","grey50"),
                         width = 1.5,marker=marker_style,
                         hovertemplate = paste(
                           
                           "%{yaxis.title.text}: %{y:.0f}<br>",
                           "%{xaxis.title.text}: %{x}<br>",
                           "<extra></extra>"
                         )) 
  
  
  generalplot2 <- generalplot2 %>% layout(title = "User Age-Group Analysis")
  generalplot2 <- generalplot2 %>% layout(legend = list(orientation = "h",xanchor = "center",  x = 0.5))
  
 # Binding the plots 
  
  
  switch(typevariable,
         usertype = generalplot1,
         useragegroup = generalplot2)
  
  




}


#Dendroram : 

## Data FOr Collapsable Tree and Top 10 Table: 
collapsabledataframe <- aggregate(accident_data_frame$FATALITY,by = list(accident_data_frame$ACCIDENT_TYPE,accident_data_frame$SPEED_ZONE),FUN=sum)
colnames(collapsabledataframe) <- c("Collision_type","SpeedZone","Fatalities")


######## A Function to Generate Collapsable tree ###################
dendogram_function <- function(){
  
  collapsabledataframe <- aggregate(accident_data_frame$FATALITY,by = list(accident_data_frame$ACCIDENT_TYPE,accident_data_frame$SPEED_ZONE),FUN=sum)
  colnames(collapsabledataframe) <- c("Collision_type","SpeedZone","Fatalities")
  collapsibleTree(collapsabledataframe,hierarchy = c("Collision_type","SpeedZone","Fatalities"),height = 2000,width = 2000,
                  tooltip = TRUE,fillByLevel = TRUE,fill="lightgreen",fontSize = 14,zoomable = T)
  
  
  

}


### Function For Day of the Week Plot FOr General Tab ######################


dayofweekplot <- function(year){
  
  
  dayofweek <- accident_data_frame[accident_data_frame$Year == as.numeric(year),]
  dayofweek2 <- data.frame(summary(dayofweek$DAY_OF_WEEK))
  dayofweek2 <- data.frame("days" = row.names(dayofweek2),dayofweek2)
  colnames(dayofweek2) <- c("Day","Count")
  rownames(dayofweek2) <- NULL
  marker_style <- list(line = list(width = 2,color = 'black'))
  generalplot3<- plot_ly(dayofweek2, x = ~Day, y = ~Count, type = 'bar',text = ~Day,color = ~ as.factor(Day),
                         width = 1.5,marker=marker_style,
                         hovertemplate = paste(
                           
                           "%{yaxis.title.text}: %{y:.0f}<br>",
                           "%{xaxis.title.text}: %{x}<br>",
                           "<extra></extra>"
                         )) 
  generalplot3 <- generalplot3 %>% layout(title = "Accidents Per Day of Week")
  generalplot3
}










########### Shiny Application Code Starts Here  #################################################################################



shinyApp(
  ui = navbarPage(title = "VICTORIAN ROAD CRASH ANALYSIS", theme = shinytheme("sandstone"),
                  tabPanel("INTRODUCTION",
                          
                           ### ------------Introduction Tab ---------------
                      
                           
                           tags$head(tags$script(HTML('var fakeClick = function(tabName) {
                                                        var dropdownList = document.getElementsByTagName("a");
                                                        for (var i = 0; i < dropdownList.length; i++) {
                                                          var link = dropdownList[i];
                                                          if(link.getAttribute("data-value") == tabName) {
                                                            link.click();
                                                          };
                                                        }
                                                      };
                                                                '))),
                             
                             
                             mainPanel(
                               
                               h1(id ="title","WELCOME TO VICTORIAN ROAD FATALITY AND INJURY ANALYSIS "),
                               h1(id ="subtitle","WELCOME TO VICTORIAN ROAD FATALITY AND INJURY ANALYSIS "),
                               tags$head(tags$style(HTML('#title { color: Tomato;font-size: 40px;font-style: bold;}'))),
                               br(),
                               br(),
                              fluidRow(
                              column(8,
                                     img(src="1.jpg", align = "center", style="width:85%; height:20%"),
                                     
                                     ),
                                column(4,align = "right",offset = 0,
                                       tags$head(tags$style(HTML('#about { color: Tomato;font-size: 20px;font-style: bold;}'))),
                                       h4(id = "about", "ABOUT THE PROJECT:",align="left"),
                                       p("There has been a decrease in road crashes eversince road safety standards have increased.However, the number of deaths, injuries and Fatalities Per 100,000 due to road crashes have increased globally. This project aims to Analyze the Key Factors responsible for Crashes to develop better roadsafety programs for the public. Hence, the key factors inspected in this project are Region, Region Comparision, Speeding, Light Condition, Crash Types, and Road USer Analysis. An Introduction of the tabs is presented below. If you wish to inspect any one of the tabs please click the buttons below.",align = "left",style = "font-size: 125%")
                                         
                                       )
                                
                              ),
                              br(),
                               br(),
                               h3("Application Tour:",style ='color:darkblue'),
                               fluidRow(
                                 column(8,align = "left",
                                        br(),
                                        img(src="GrandFinal.gif", align = "left"),
                                        
                                      
                                        
                                        ),
                                 
                                 column(4,align = "left", offset = 0,
                                        
                                        h3(id="subtitle", "INTRODUCTION :",align = "center"),
                                        tags$head(tags$style(HTML('#subtitle { color: red;font-size: 20px; font-style: bold;}'))),
                                        tags$head(
                                          tags$style(HTML('#regionbutton{background-color:orange}'))
                                        ),
                                        
                                        
                                        tags$head(
                                          tags$style(HTML('#tab3{background-color:orange}'))
                                        ),
                                        
                                        
                                        
                                        
                                        
                                        
                                        tags$head(tags$style(HTML('#one { color: orangered;font-size: 20px; font-style: bold;}'))),
                                        
                                        
                                        br(),
                                        h4(id = "one" ,"Welcome to Victorian Road Crash Analysis. A Short Tour of the Application is provided below:",align = "left"),
                                        br(),
                                        h4( id = "2","Please Click this button to go to Region Analysis:",style = 'color:darkblue',align ="left"),
                                        actionButton("regionbutton","TAB 1",onClick= "fakeClick('REGION ANALYSIS')",align = "center"),
                                        br(),
                                        h4(id="3","Please Click this Button to go to Road Geometry and Speed Zone Analysis:",style = 'color:darkblue',align="left"),
                                        actionButton("tab2button","TAB 2",onClick= "fakeClick('SPEEDZONE/ ROAD GEOMETRY')",align="center"),
                                        br(),
                                        h4(id = "4","Please Click this Button to go to General Analysis : ",style = 'color:darkblue',align="left"),
                                        actionButton("tab3","TAB 3",onClick= "fakeClick('GENERAL ANALYSIS')",align="center"),
                                        br(),
                                        h4(id = "5","Please Click this Button to analyse the correlation between Crashtype and Speed Zone ",style = 'color:darkblue',align="left"),
                                        actionButton("tab4","TAB 4",onClick= "fakeClick('CRASH TYPE +  SPEED ZONE ANALYSIS')",align = "center")
                                        
                                        )
                                 
                                 
                                 
                               ),
                               
                               
                             )
                           ),
                 
                  
                  ################## TAB 1 ##########################
                  ############## Region Analyisis #################
                  
                  tabPanel("REGION ANALYSIS",
                           
                           sidebarLayout(
                             sidebarPanel(width = 2,
                               h4("USER GUIDE:"),
                               br(),
                               h4("Welcome to Region Analysis:"),
                               p("About:"),
                               p("This Section Helps You Analyze the accidents in various regions in victoria"),
                               p("Also, a comparision between Rural and Metro victoria  is shown"),
                               br(),
                               h4("Please select the choice of Analysis"),
                               p("You can Analyze either the Accidents or Injury/Fatality "),
                               radioButtons("pType", "Distribution type:",
                                            c("Injury/Fatality" = "A",
                                              "Accident" = "B")),
                               br(),
                               h4("Choose an option for Metro vs Rural Victoria Comparision"),
                               
                               radioButtons("option_2", "Metro vs Rural Comparision:",
                                            c("Injury" = "C",
                                              "Fatality" = "D")),
                               br(),
                               h4("KEY TAKEAWAYS:"),
                               p("1.Over the period of Six Years, Metro Politan North West Region has the highest number of Accidents."),
                               p("2.Overall, there has been a decrease in the number of Fatalities in Metropolitan and rural areas."),
                               p("3.Moreover, the  number of alcoholic accidents in Metro and Rural Victoria have reduced significantly over the years. "),
                               p("4.Overall, the western region Has the least Number of Accidents as it is a Non Subrban Region")
                               
                               
                               
                               
                               
                               
                             ),
                             mainPanel(
                               
                              
                               
                               
                               
                               
                               
                               h2("REGION ANALYSIS "),
                               
                              br(),
                              
                              h4("PLEASE HOVER OVER THE BARS FOR THE INFORMATION:"),
                              h6("Click on the Barplot for Further Analysis:"),
                              
                               
                               
                               
                               fluidRow(
                                 
                                 column(6,align = "left",
                                        
                                        withSpinner(plotlyOutput('plot')),
                                        br(),
                                        br(),
                                        br(),
                                        h4("Alcoholic and Unlicensed Accidents in this Region are:"),
                                        tableOutput("table")
                                        
                                        
                                       
                                        
                                        
                                 ),
                                 
                                 column(6,align = "right",
                                        h3("Hover over the Bars for Clear Information"),
                                        h4("Click on the barplot for further analysis"),
                                        withSpinner(plotlyOutput('plot2')),
                                        br(),
                                        br(),
                                        br(),
                                        h4("The table below displays the Alcoholic and Unlicensed Accidents Per Year"),
                                       tableOutput("table2")
                                        )
                                 


                               )
                               
                               
                               
                             )
                           )
                  ),
                  
                  
                            
                                  
                                      
                                      
                
                #################### TAB - 2 ##################################################################################################         
                           
                ########### SPeed Zone / Road Geometry Analysis ###########################
                
                  tabPanel("SPEEDZONE/ ROAD GEOMETRY",
                           h2("Welcome to Speed Zon/Road Geometry and Light Condition Analysis"),
                           br(),
                           
                          sidebarLayout(
                             sidebarPanel(width = 2,
                                          h4("USER GUIDE:"),
                                          br(),
                                          h4("Welcome to Speed Zone/Road Geoemtry /Light Condition Analysis:"),
                                          p("About:"),
                                          p("This Section Helps You Analyze the accidents per Speed Zone and Road Gemeotry in victoria"),
                                          p("Also, this Section Focuses on Light Condtion Analysis as well"),
                                          br(),
                                          h4("Please select the choice of Analysis"),
                                          p("You can Analyze either Speed Zone or Road Gemeotry"),
                                          selectizeInput("speedzonechoice", "Analysis type:",
                                                       c("Speed Zone" = "speedzone",
                                                         "Road Geometry" = "geometry")),
                                          br(),
                                          p("Please Hover on the plots for Detailed Information"),
                                          p("Interactions:"),
                                          p("1.Click on The Barplot to get Per Year Alcohol vs Unlicensed Analysis"),
                                          p("2.Click on the Donut Chart to get Per Year Light Analysis"),
                                          br(),
                                          h5("KEY TAKEAWAYS:"),
                                          p("1.Suprisingly,60kmph SpeedZone has the most number of Accidenst and Injuries.",align = "justify"),
                                          p("This maybe due to overspeeding and the extensive usage of State Highways !!",align = "justify"),
                                          p("2. The 100Kmph SpeedZone has the most number of Fatalities.",align = "justify"),
                                          p(" 3. Most of the Accidents are during the Day as most of the people travel during the day.",align = "justify"),
                                          p(" 4. The number of Accidents are reducing per year with regard to Light Condition and Speed Zone.")

 
                                            
                                            
                                          
                                          
                             ),
                             
                             mainPanel(
                               h2("Welcome to speedzone/roadgeometry analysis"),
                               
                               column(6,align = "lefft",
                                      
                                      h4("Please Hover over the Bars for Clear Information: "),
                                      h3("Please Click on the Bars to Visualise Further !!"),
                                      withSpinner(plotlyOutput("speedplottab2")),
                                      br(),
                                      p("This is a per year analysis for Alcohol vs Unlicensed"),
                                      withSpinner(plotlyOutput('subscatteroutput')),
                                      p("These accidents are reducing as time progresses"),
                                      p("People are Becoming Aware !!")
                                    
                                      
                                      
                                      ),
                              
                               column(6,align = "right",
                                      h3("Here's some Analysis of Light Conditons:"),
                                      p("Please Hover Over the Donut Chart for Light Condition Information",align = "justify"),
                                      p("Please click the chart for further Analysis",align = "justify"),
                                      withSpinner(plotlyOutput("donut")),
                                      p("Well !! Majority of the Accidents were during the Day",align = "justify"),
                                      br(),
                                      withSpinner(plotlyOutput('subbarplot'))
                                      )
                             )
                           
                  )
                  
                  
                  ),
                  
                  
                  
                  
                  
      
                
                
                
      ################# TAB 3 ##########################################################################
                ################# General Analysis ############################
                  tabPanel("GENERAL ANALYSIS",
                           h2("General Analysis"),
                               br(),
                               sidebarLayout(
                                 sidebarPanel(width = 2,
                                              h4("USER GUIDE:"),
                                              br(),
                                              h4("Welcome to General Analysis:"),
                                              p("About:"),
                                              p("This Section Helps You Analyze the accidents per Road User Type/AgeGroup ,Injuries Fatalities Per Year"),
                                              p("Also, this Section Focuses on Injuries/Fatalities per year and Accidents Per Day"),
                                              br(),
                                              br(),
                                              h4("Please select the choice of Analysis"),
                                              p("You can Analyze either accidents by user type or user agegroup"),
                                              radioButtons("usertypechoice", "Road User Analysis:",
                                                             c("Road User Type" = "usertype",
                                                               "User Age Group" = "useragegroup")),
                                              br(),
                                              p("Please Hover on the plots for Detailed Information"),
                                              h4("Please Select Your Choice of Analysis for the Lineplot"),
                                              actionButton("fatalityaction","Fatality"),
                                              actionButton("injuryaction","Injury"),
                                              actionButton("bothaction","Both"),
                                              br(),
                                              br(),
                                              h4("Choose An Year for the Accidents Per Day Analysis"),
                                              selectInput("yearchoice", "Select Year for Analysis:",
                                                             c("2013" = "2013",
                                                               "2014" = "2014",
                                                               "2015" = "2015",
                                                               "2016" = "2016",
                                                               "2017" = "2017",
                                                               "2018" = "2018",
                                                               "2019"  = "2019"),selected = "2013")
                                              
                                              
                                              
                                 ),
                                 
                                 mainPanel(
                                   h2("Welcome to general Analysis"),
                                   br(),
                                   fluidRow(
                                   column(width = 6,align = "left",offset = 0.8,
                                          h5("Road USER Analysis"),
                                          h6("Please Hover over the bars for Additional Information"),
                                          
                                          plotlyOutput('genralplot_1')
                                        
                                          ),
                                  
                                   
                                   
                                   
                                   column(width=6,align = "right",offset = 0.8,
                                          h3("Injuries/Fatalities Per Year"),
                                          h6("Please Hover over the points for clear information"),
                                          plotlyOutput('general_2')
                                          
                                          
                                          )),
                                   
                                   
                                   fluidRow(
                                     
                                     column(width=6,align = "left",
                                            
                                            h5("Analyzing accidents Per Day for Various years:"),
                                            plotlyOutput("generalplot3")
                                          
                                            
                                            
                                            ),
                                     column(4,align = "right",offset = 2,
                                            h4("KEY FINDINGS:",align = "right",style = 'color:darkblue'),
                                            p("1.Users between the age 30-39 are affected the most as the working population largely lies in that age group",style = 'color:darkblue',align = "justify"),
                                            p("2. Drivers are most affected by the accidents as opposed to pillions, pedestrians and other road users. This can be related to crash type as collision with vehicle is the highest, the drivers would be effected most.",align = "justify",style = 'color:darkblue',),
                                            p("3.Injuries/Fatalities are decreasing as years progress",align = "justify",style = 'color:darkblue'),
                                            p("4.Over the last 6 years Friday and Sunday have the most number of accidents",style = 'color:darkblue',align = "justify")
                                            )
                                     
                                     
                                     
                                   )
                                   
                                 )
                                 
                               )
                           
                  ),

################ Tab 4 ######################################
#################### Association Between Speed ZOne and Crash Type ###############################################

                   tabPanel("CRASH TYPE +  SPEED ZONE ANALYSIS",
                            mainPanel(
                              
                              
                              h2("Crash Type Analysis AND Speed Zone Analysis :",style='color:darkblue'),
                              p("Please Click the Nodes for the Crashtype/SpeedZone Analysis"),
                              p("Also, you can Hover over the Nodes to read the Names Clearly"),
                              br(),
                              
                              fluidRow(
                                column(width = 12,
                                  
                                       collapsibleTreeOutput("dendogram"),
                                       br()
                                  
                                )
                                
                                ),
                            
                            h4("As the Dendogram is Congested, Please Have a Look at this Table !!"),
                            h6("Top Five Accident Type and Speed Zone for Fatalities:"),
                              
                              fluidRow(
                                column(width = 4,align = 'right',offset = 2,
                                
                                       tableOutput("collapsabletable")
                                  
                                  
                                ),
                                
                                column(width = 5,align = "Left",
                                       
                                       h4("KEY TAKEAWAYS",style='color:darkblue'),
                                       p("1. The Collision type, Struck With Animal, is the least fatal accident",align = "justify",style = "color:darkblue"),
                                       p("2. The COllision with Vehicle is the most Fatal Accident in 100km/hr speed zone",align = "justify",style = "color:darkblue"),
                                       p("3.Moreover, the Collision with a fixed Object is the most Fatal Accident in the 100 km/hr Speed Zone",align = "justify",style = "color:darkblue")
                                       
                                       )
                                
                               
                                
                                )
                                
                                
                              

                   )


                   )               
                               
                             
                             
                             
                             
                             
                             
                           
                           
                           
                           
  
),

################ Server Code ##########
  
server = function(input, output) {
    
  ######### Plot 1 Tab 1 
    output$plot <- renderPlotly({ 
      plotType(input$pType)
    
      
    })
    
    
    
    
    
    ### ON click table for plot 1 
    output$table <- renderTable({
      s <<- event_data("plotly_click", source = "event1plot1")
      if (is.null(s)) {
        return(NULL)
      }
      else{
        
        return(regions_stats[regions_stats$Region == as.character(s[3]),])
        
        
      }
        
          
        
      
      
    })
  
  ################### Plot  2 Tab 1 
    
     
     output$plot2 <- renderPlotly({
       plotType_2(input$option_2)
      })
    
     
     
     ############# On click table for Plot 2
     output$table2 <- renderTable({
       s2 <<- event_data("plotly_click", source = "event1plot2")
       if (is.null(s2)) {
         return(NULL)
       }
       else{
         
         return(tab1plot2subdf[tab1plot2subdf$Year == as.numeric(s2[3]),])


       }
})

############################### Plot 1 Tab 2 
     output$speedplottab2 <- renderPlotly({
       plotType_tab2(input$speedzonechoice)
     })
     
     ######################## Plot 2 Tab 2 
     output$donut <- renderPlotly({
       
       
       donutfig <- donutdata %>% plot_ly(labels = ~category, values = ~count,source = "donutclick",key= ~category)
       donutfig <- donutfig %>% add_pie(hole = 0.6)
       donutfig <- donutfig %>% layout(title = "LIGHT CONDITION ANALYSIS",  showlegend = F,
                                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
       
       
       
       
       donutfig
       
     })
     
    ################# On click for plot 2  
     output$subbarplot<- renderPlotly({
       s3 <<- event_data("plotly_click", source = "donutclick")
       if (is.null(s3)) {
         
         
         subdonut <- donuttdata21[donuttdata21$Light_condition  == "Day",]
         subdonut <- data.frame("a" = subdonut$Year)
         subdonut<- subdonut %>%group_by(a) %>% count() 
         
         colnames(subdonut) <- c("Year","count")
         
         
         fig <- plot_ly(subdonut,
                        x = ~as.factor(Year),
                        color = ~ as.factor(Year), 
                        y = ~count,
                        type = "bar" )
         
         
         
         
         fig<- fig %>% layout(title = "Accident Analysis Per Light Condition", xaxis = list(title = "Year"),yaxis = list(title = "Accident"))
         fig
         
         
         
         
         
         
         
         
       }
       else{
         
         subdonut <- donuttdata21[donuttdata21$Light_condition  == s3$key,]
         subdonut <- data.frame("a" = subdonut$Year)
         subdonut<- subdonut %>%group_by(a) %>% count() 
         
         colnames(subdonut) <- c("Year","count")
         
         
         fig <- plot_ly(subdonut,
                        x = ~as.factor(Year),
                        color = ~ as.factor(Year), 
                        y = ~count,
                        type = "bar" )
         
         
         
         
       fig<- fig %>% layout(title = "Accident Analysis Per Light Condition", xaxis = list(title = "Year"),yaxis = list(title = "Accident"))
       fig
        
         
         
       }
       
     })
     
     ############# On click for plot 1  tab 2 : 
     
     
     
     output$subscatteroutput<- renderPlotly({
       s44 <<- event_data("plotly_click", source = "subline")
       if (is.null(s44)) {
         plotType_subplot("60 km/hr")
       }
       else{
         print(s44$key)
         plotType_subplot(s44$key)
         
         
         
       }
       
     })
     
     
     
     
     ############ Plot 1 Tab 3 
     output$genralplot_1 <- renderPlotly({
       
       plotType_tab3_generalplot(input$usertypechoice)
       
       
     })
  
     #### General analysis line plot code: Plot 2 Tab 3 
     
     tab3lineclick <- reactiveValues(plot11 = NULL)
     #### Event for both plots: 
     observeEvent(input$bothaction,
       {
       tab3lineclick$plot11 <- year_analysis_both()
     }
     )
     
     #### Event for Injury action buttuon 
     observeEvent(input$injuryaction,{
       tab3lineclick$plot11 <- year_analysis_injury()
     }
     )
     
     
     #### Event for Fatality action button  :
     
     #### Event for both plots: 
     observeEvent(input$fatalityaction,{
       tab3lineclick$plot11 <- year_analysis_fatality()
     }
     )
     
     
     
     output$general_2 <- renderPlotly({
       if (is.null(tab3lineclick$plot11)){
         return(year_analysis_both())
       } 
       tab3lineclick$plot11
     })
     
                  
      
      
      
      output$dendogram <- renderCollapsibleTree({
      
         dendogram_function()
        })
      
      
      output$collapsabletable <- renderTable({
        topdata <- collapsabledataframe[order(-collapsabledataframe$Fatalities),]  
        top5data <- head(topdata,5)
        top5data
        
        
      })
      
      
      output$generalplot3 <- renderPlotly({
        
        
        dayofweekplot(input$yearchoice)
        
        
      })

                  
                  
     
   
  }


)



