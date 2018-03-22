#Loading the file for cleaning
setwd("E:/zappos")

#Imporitng Libraries 
library(readxl)
library(VIM)
library(outliers)
library(dplyr)

#Imporing Data
Sales <- read_excel("challenge.xlsx")
class(Sales)
View(Sales)

#Checking NA values
any(is.na.data.frame(Sales))
sum(is.na.data.frame(Sales))

Sales<-na.omit(Sales)
View(Sales)
any(is.na.data.frame(Sales))

#Checking Datatype for each column
sapply(Sales, mode)

#Changing Stock Code to Integer from character as mentioned in attributes details
Sales<-transform(Sales,  StockCode= as.numeric(StockCode))
any(is.na.data.frame(Sales))
Sales<-na.omit(Sales)

#Dividing  Invoice Date into two clomns of date and time
Sales<- separate(Sales,InvoiceDate,into = c("Year","Month","Day"),sep = "-",remove = FALSE)
Sales<- separate(Sales,Day,into = c("Date","Time"),sep = " ",remove = TRUE)
Sales<-Sales[c(1:4,6:12)]

#Checking Outliers on Quantity
outlier(Sales['Quantity'], opposite = FALSE, logical= FALSE)
boxplot(Sales$Quantity,horizontal = TRUE)
#Cleaned_sales <-filter(Sales, Quantity < 10000)
#Cleaned_sales <-filter(Cleaned_sales, Quantity > -10000)
#View(Cleaned_sales)
#boxplot(Cleaned_sales$Quantity,horizontal = TRUE)

#Creating new column Quantity Price
Cleaned_sales<-Sales
Cleaned_sales$Quantity_Price<-Cleaned_sales$Quantity*Cleaned_sales$UnitPrice

#submission of cleaned data into excel file
write.csv(Cleaned_sales,"E:/zappos/cleaned_data.csv",row.names=F)

#Creating two dataframes for canelled orders and placed orders
Placed_order<-filter(Cleaned_sales, Quantity_Price > 0)
Cancelled_order<-filter(Cleaned_sales,Quantity_Price < 0)

Placed_order$Placed_Invoice<-Placed_order$Quantity_Price
Placed_order$Quantity_Price <- NULL

Cancelled_order$Cancelled_Invoice<-Cancelled_order$Quantity_Price
Cancelled_order$Quantity_Price <- NULL

View(Placed_order)
View(Cancelled_order)

#submission of data into excel file
write.csv(Placed_order,"E:/zappos/ordered_data.csv",row.names=F)
write.csv(Cancelled_order,"E:/zappos/cancelled_data.csv",row.names=F)
