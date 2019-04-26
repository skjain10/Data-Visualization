h1b <- read.csv("H1_r.csv",sep="," ,header=TRUE,stringsAsFactors = FALSE)
nrow(h1b)

getwd() #"H-1B_Disclosure_Data_FY17.csv"
h1b <- h1b[which(h1b$VISA_CLASS == 'H-1B'),]
View(h1b)
nrow(h1b)
h1b <-h1b[which(h1b$EMPLOYER_COUNTRY=='UNITED STATES OF AMERICA'),]
nrow(h1b)

i <- c(1,2,8,11,12,22,34,35,50)
h1b <- h1b[i]

h1b$PREVAILING_WAGE <- gsub("00$","", h1b$PREVAILING_WAGE)
h1b$PREVAILING_WAGE <- gsub(".$","", h1b$PREVAILING_WAGE)
h1b$PREVAILING_WAGE <- gsub(",","", h1b$PREVAILING_WAGE)

h1b <- h1b[which(h1b$PW_UNIT_OF_PAY=='Year'),]



h1b$PREVAILING_WAGE <- as.numeric(h1b$PREVAILING_WAGE)
str(h1b)
options(scipen=99)
plot(h1b$PREVAILING_WAGE,ylim = c(0,800000), ylab="Prevailing Wage", xlab="No.of Records")
abline(h=mean(h1b$PREVAILING_WAGE), col="red", lwd=3, lty=2)
?plot

library(plotrix)
pie3D(x4 ,explode=0.1 ,col=c("#C9B1AD","#DC7627","#C9625D","#000CE5"))

?pie3D

x3<-table(h1b$CASE_STATUS)
x4 <- c(529092,41484,6160,18997)

boxplot(h1b$PREVAILING_WAGE, ylim =c(0,200000),col="#C67B7B",ylab="Prevailing Wage", main= "Distribution of Prevailing Wage")
abline(h=mean(h1b$PREVAILING_WAGE), col="#161142", lwd=3, lty=2)

?boxplot
library(ggplot2)

blank_theme <- theme_minimal()+ theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

library(scales)
pie(status) + scale_fill_grey() +  blank_theme +
  theme(axis.text.x=element_blank()) 
library(Rcpp)
library(ggplot2)


status <- data.frame(
  group = c("Certified", "Certified-Withdrawn", "Denied","Withdrawn"),
  value = c(529092, 41484, 6160, 18997)
)

bp<- ggplot(status, aes(x="", y=value, fill=group))+ geom_bar(width = 1, stat = "identity")


pie <- bp + coord_polar("y", start=0)
library(scales)
pie + scale_fill_grey() +  blank_theme +
  theme(axis.text.x=element_blank())+ geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                                                   label = percent(value/v)), size=5)

short <-(table(h1b$WORKSITE_STATE))
short[-1]
?table



state_full <- state.name


work <- c(1601,125,9363,3338,
          115537,6764,8226,3689,
          21046,21930,421,851,
          29813,6266,3102,2530,
          2568,1596,597,9815,
          24129,18747,10249,712,
          8225,175,2309,1425,
          1552,35570,854,49249,
          17352,24129,14477,1735,
          4168,24264,2092,3049,
          332,6901,59997,2988,
          374,17102,25154,454,
          6187,136)

datamap <- data.frame(state_full,work)
datamap$state<- tolower(datamap$state_full)

us <- map_data("state")

map.area <- ggplot(merge1,aes(map_id = state))   

map.area <- map.area +  geom_map(map = us,aes(fill=work))   #Fill in with state_area

map.area <- map.area + expand_limits(x = us$long, y = us$lat)  #Maps the coordinate and gives the title  

map.area <- map.area + coord_map() +  ggtitle("")  + 
  scale_fill_continuous(
    low = "white", high = "#D26757", label = scales::comma
  )
#Zoom in and Zoom out clearly

map.area <- map.area + geom_point(aes(x=state_center_x,y=state_center_y,size=Pre_Wage),col="#252452")

map.area
datamap$state_center_x<- state.center$x

datamap$state_center_y<- state.center$y
View(datamap)



library(dplyr)


by_state <- group_by(h1b,WORKSITE_STATE)
by_state_prewage<-summarise(by_state,Pre_Wage=mean(PREVAILING_WAGE))
by_state_prewage$stateabb <- by_state_prewage$WORKSITE_STATE


merge1<-merge(datamap,by_state_prewage,by="stateabb") 

View(merge1)

by_state_prewage$Pre_Wage[-1]

View(by_state_prewage)
str(h1b)

sort(table(h1b$WORKSITE_STATE))

str(by_state_prewage)
View(datamap)
datamap$stateabb <- state.abb


ggplot(data=h1b, aes(x=, y=len, fill=supp)) +
  geom_bar(stat="identity")
# Use position=position_dodge()
ggplot(data=df2, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge())

h1b_top <- sort(table(h1b$JOB_TITLE))
h1b_top10 <- tail(h1b_top,n=15)
par(mar=c(15,5,5,5)) 
barplot(h1b_top10,las=2,col= "#C9625D") 





by_jt <- group_by(h1b,JOB_TITLE)
by_jt_wage<-summarise(by_jt,count=n())
View(by_jt_wage)

str(h1b)

s <- c("shwet","shwet","shwet 123.45","shwet", "yash", "yash")

y <- c(4,4,3.72,3.75,4,4)

sy<- data.frame(s,y)

by_main <- group_by(sy,s)
by_main1<-summarise(by_main,avg=mean(y),count=n())

table(sy$s)



h1b_SOFT <-h1b[which(h1b$JOB_TITLE=="SOFTWARE ENGINEER"),]
nrow(h1b_SOFT) 


myvars <- h1b$EMPLOYER_NAME %in% c("TECH MAHINDRA (AMERICAS),INC. ", "GOOGLE LLC", "CAPGEMINI AMERICA INC","WIPRO LIMITED","ACCENTURE LLP","ERNST & YOUNG U.S. LLP","COGNIZANT TECHNOLOGY SOLUTIONS US CORP","INFOSYS LIMITED","TATA CONSULTANCY SERVICES LIMITED","DELOITTE CONSULTING LLP",'TECH MAHINDRA (AMERICAS),INC.') 
newdata <- h1b$EMPLOYER_NAME[myvars]
new_data <- h1b$PREVAILING_WAGE[myvars]
data_new <- h1b$CASE_STATUS[myvars]
dataframe_go <- data.frame(newdata,new_data,data_new)
View(dataframe_go)
dataframe_go <- within(dataframe_go, 
                       newdata <- factor(newdata, 
                                         levels=names(sort(table(newdata), 
                                                           decreasing=FALSE))))

ggplot(dataframe_go, aes(x=newdata,fill=data_new)) + geom_bar()+ coord_flip()

 


myvars <- h1b$EMPLOYER_NAME %in% c("TECH MAHINDRA (AMERICAS),INC. ", "GOOGLE LLC", "CAPGEMINI AMERICA INC","WIPRO LIMITED","ACCENTURE LLP","ERNST & YOUNG U.S. LLP","COGNIZANT TECHNOLOGY SOLUTIONS US CORP","INFOSYS LIMITED","TATA CONSULTANCY SERVICES LIMITED","DELOITTE CONSULTING LLP",'TECH MAHINDRA (AMERICAS),INC.') 
newdata <- h1b$EMPLOYER_NAME[myvars]
new_data <- h1b$PREVAILING_WAGE[myvars]

dataframe_go <- data.frame(newdata,new_data)
View(dataframe_go)
dataframe_go <- within(dataframe_go, 
                       newdata <- factor(newdata, 
                                        levels=names(sort(table(newdata), 
                                                           decreasing=FALSE))))
qwerty<- sort(table(h1b$EMPLOYER_NAME))
yu <- tail(qwerty,n=10)

ggplot(dataframe_go, aes(x=newdata,fill=new_data)) + geom_bar()+ coord_flip()
library(dplyr)

str(dataframe_go)

by_jt <- group_by(dataframe_go,newdata)
by_jt_wage<-summarise(by_jt,count=n(),average=mean(new_data))
View(by_jt_wage)
str(by_jt_wage)
poiu <- by_jt_wage[order(by_jt_wage$count),] 



par(mar=c(5,10,5,5)) 
barplot(poiu$average,names.arg = poiu$newdata,las=2,horiz = TRUE,xlim = c(0,150000))
lines(by_jt_wage$average)

barplot(by_jt_wage$newdata,by_jt_wage$average)

?barchar

lines(by_jt_wage$newdata,by_jt_wage$average,type="l")
points(by_jt_wage$newdata,by_jt_wage$average,type="p")







myvars <- h1b$JOB_TITLE %in% c("SOFTWARE DEVELOPER", "SOFTWARE ENGINEER", "PROGRAMMER ANALYST","ASSISTANT PROFESSOR","CONSULTANT","ASSISTANT PROFESSOR","BUSINESS ANALYST","PROJECT MANAGER")
newdata <- h1b$JOB_TITLE[myvars]
new_data <- h1b$PREVAILING_WAGE[myvars]
data_new <- h1b$CASE_STATUS[myvars]
dataframe_go <- data.frame(newdata,new_data,data_new)
View(dataframe_go)
dataframe_go <- within(dataframe_go, 
                       newdata <- factor(newdata, 
                                         levels=names(sort(table(newdata), 
                                                           decreasing=FALSE))))

ggplot(dataframe_go, aes(x=newdata,fill=data_new)) + geom_bar()+ coord_flip()

boxplot(PREVAILING_WAGE~CASE_STATUS,
        data=h1b,
        main="Different boxplots for VISA Status",
        ylab="$Yearly",
        col="orange",
        border="brown",
        ylim= c(0,200000)
)

h1b_Certified <- h1b[which(h1b$CASE_STATUS=='CERTIFIED'),]
View(h1b_Certified)

abline(h=mean(h1b_Certified$PREVAILING_WAGE), col="#161142", lwd=3, lty=2)


h1b_Denied<- h1b[which(h1b$CASE_STATUS=='DENIED'),]
View(h1b_Denied)

abline(h=mean(h1b_Denied$PREVAILING_WAGE), col="white", lwd=3, lty=2)

mean(h1b_Denied$PREVAILING_WAGE)
mean(h1b_Certified$PREVAILING_WAGE)

boxplot(h1b_Denied$PREVAILING_WAGE, ylim =c(0,300000),col="#C67B7B",ylab="Prevailing Wage", main= "Distribution of Prevailing Wage")
abline(h=mean(h1b_Denied$PREVAILING_WAGE), col="#161142", lwd=3, lty=2)


x <- sort(table(h1b$EMPLOYER_NAME))
y <- tail(x,n=15)


library(lattice)

Total<- c(618804,647852,624650,654360)
Total1 <- c(536679,558001,452186,566979)
Total2 <- c(9702,8197,6230,7351)

Total

year <- data.frame(Total,Total1,Total2)
View(year)
colnames(year) <- c("Total","Total1","Total2")
rownames(year) <- c(2015,2016,2017,2018)

plot(year$Total, type= "l",ylim = c(400000,800000))
points(year$Total, type= "p",ylim = c(400000,800000))

lines(year$Total1, type= "l",ylim = c(400000,800000))
points(year$Total1, type= "p",ylim = c(400000,800000))



plot(year$Total2, type= "l",ylim = c(0,10000))
points(year$Total2, type= "p",ylim = c(0,10000))




abcd <- c("Database","Database","Database","Software","Software","Software","Software","Software","Database","Database")
pqrs <- c(2016,2016,2017,2017,2017,2016,2018,2018,2018,2018)

df45 <- data.frame(abcd,pqrs)




list1 <- newdata
list2 <- rep(2018,length(list1))
list3 <- cbind(list2, list1)

  
list3 <- data.frame(date = list2, number = list1)
View(list3)








myvars <- h1b$JOB_TITLE %in% c("SOFTWARE DEVELOPER", "PROGRAMMER ANALYST","ASSISTANT PROFESSOR","CONSULTANT","ASSISTANT PROFESSOR","BUSINESS ANALYST","PROJECT MANAGER")
newdata <- h1b$JOB_TITLE[myvars]

list1 <- newdata
list2 <- rep(2018,length(list1))
list2018 <- cbind(list2, list1)
list2018 <- data.frame(date = list2, number = list1)
View(list2018)



myvars <- h1b$JOB_TITLE %in% c("SOFTWARE DEVELOPER","PROGRAMMER ANALYST","ASSISTANT PROFESSOR","CONSULTANT","ASSISTANT PROFESSOR","BUSINESS ANALYST","PROJECT MANAGER")
newdata <- h1b$JOB_TITLE[myvars]

list1 <- newdata
list2 <- rep(2017,length(list1))
list2017 <- cbind(list2, list1)


list2017 <- data.frame(date = list2, number = list1)
View(list2017)


myvars <- h1b$JOB_TITLE %in% c("SOFTWARE DEVELOPER","PROGRAMMER ANALYST","ASSISTANT PROFESSOR","CONSULTANT","ASSISTANT PROFESSOR","BUSINESS ANALYST","PROJECT MANAGER")
newdata <- h1b$JOB_TITLE[myvars]


list1 <- newdata
list2 <- rep(2016,length(list1))
list2016 <- cbind(list2, list1)


list2016 <- data.frame(date = list2, number = list1)
View(list2016)

myvars <- h1b$JOB_TITLE %in% c("SOFTWARE DEVELOPER","PROGRAMMER ANALYST","ASSISTANT PROFESSOR","CONSULTANT","ASSISTANT PROFESSOR","BUSINESS ANALYST","PROJECT MANAGER")
newdata <- h1b$JOB_TITLE[myvars]

list1 <- newdata
list2 <- rep(2015,length(list1))
list2015 <- cbind(list2, list1)
list2015 <- data.frame(date = list2, number = list1)
View(list2015)

finaltrend <- rbind(list2015,list2016,list2017,list2018)


histogram(~finaltrend$date| finaltrend$number,type= "count")
densityplot(~finaltrend$date| finaltrend$number )


a123 <- sort(table(h1b$JOB_TITLE))
tail(a123,n=15)


library(lattice)

xyz <- h1b$JOB_TITLE
View(xyz)


library(sm)
attach(mtcars)



plot(density(h1b$CASE_STATUS))

unique(h1b$JOB_TITLE)



