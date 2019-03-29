# The Loan file was initially screened in Excel and some repeated patterns were identified before importing the data in 'R'
# Hence based on those findings we have made some assumptions on required columns v/s columns not required due to following observed points.
### a. Large number of Columns have only 'NA' values
### b. Some Columns have combination of '0' & 'NA' values where in '0' has no significant relevance to the data required for Analysis.
### c. Some Columns have only one value repeated from 1st to last row, this also has no significant relevance to required data for Analysis.


# Loading the Loans database into R and checking the structure and summary
loan_dataset <- read.csv("loan.csv", stringsAsFactors = FALSE)
str(loan_dataset)
summary(loan_dataset)
# Total number of observations in the data set are 39717

################ DATA CLEANING ################

# Lets find out the number of NA's values per column in the entire dataset
library(dplyr)
missing_values_1 <- loan_dataset %>%
  select(everything()) %>%
  summarise_all(funs(sum(is.na(.))))

#  Based on checking this data carefully we realise that our findings in excel are the same when checking in 'R'
## Based on the learnings shared above, which were derived from screening the data in Excel, we will proceed with removing the unnecessary columns 
#  since they don't seem relevant to the scope of the Analysis

colnames(loan_dataset)
loan_dataset <- loan_dataset[, -c(18,36,50:105,107:111)]
str(loan_dataset)
summary(loan_dataset)

# There a few set of columns that seem to be potential factor values hence
# converting columns - term, grade, emp_length, home_ownerswhip, verification_status, loan_status, purpose, addr_state into factors
loan_dataset[, c("term", "grade", "emp_length", "home_ownership", "verification_status", "loan_status", "purpose", "addr_state")] <- lapply(loan_dataset[, c("term", "grade", "emp_length", "home_ownership", "verification_status", "loan_status", "purpose", "addr_state")],factor)
str(loan_dataset)
summary(loan_dataset)

# Creating a couple of plots to check for more missing Values
# install.packages("DataExplorer")
library(DataExplorer)

plot_missing(loan_dataset)

# install.packages("naniar")
library(naniar)
# install.packages("UpSetR")
library(UpSetR)


# Also Checking to see any interrelation/simultaneous occuring of missing values between multiple columns.

loan_dataset %>%
  as_shadow_upset() %>%
  upset()

# Counting number of NA values for all remaining columns inorder to decide if the columns needs to be kept or removed

missing_values_2 <- loan_dataset %>%
  select(everything()) %>%
  summarise_all(funs(sum(is.na(.))))


# Finding percentage of missing values for two columns with highest number of missing values.
percent_of_miss1 <- (missing_values_2$mths_since_last_delinq/39717)*100
percent_of_miss2 <- (missing_values_2$mths_since_last_record/39717)*100
percent_of_miss3 <- (missing_values_2$pub_rec_bankruptcies/39717)*100

# With such a large percentage of values missing for first two columns infering new values could mislead or exagerrate the information hence leaving NA values
# intact for these two columns
#For third column, however, we have only 1.75% missing values, hence we can infer missing values with most repeated values in the column
Mode <- function(x) {
  uniquex <- unique(x)
  uniquex[which.max(tabulate(match(x, uniquex)))]
}
pub_rec_bankruptcies_miss <- Mode(subset(loan_dataset, loan_dataset$pub_rec_bankruptcies != "NA")$pub_rec_bankruptcies)
pub_rec_bankruptcies_miss

loan_dataset$pub_rec_bankruptcies[which(is.na(loan_dataset$pub_rec_bankruptcies))] <- pub_rec_bankruptcies_miss
summary(loan_dataset$emp_length)

#Finding number of blank observations in the entire database in order to infer any additional categorical values if necessary.

blank_values_1 <- sapply(loan_dataset,function(x) table(as.character(x) =="")["TRUE"])
blank_values_1

#This shows the number of blank cells in respective columns
# emp_title - 2453
# desc - 12939
# title - 9
# revol_util - 50
# last_pymnt_d - 71
# next_pymnt_d - 38577

# Following columns need to be cleaned up by removing unnecessary characters or symbols,inferring values where necessary & changing formats where applicable.
# int_rate, revol_util, emp_length, emp_title, desc, title, earliest_cr_line,last_pymnt_d, next_pymnt_d, last_credit_pull_d, 

# First converting percentage values into numeric values
loan_dataset$int_rate <-  as.double(sub("%","",loan_dataset$int_rate))
loan_dataset$revol_util <-  as.double(sub("%","",loan_dataset$revol_util))

# It is also known that revol_until also has 50 blank values, since it is a relatively small number compared to total number of observations hence inferring the
# blank values with mode of the column
Mode1 <- function(x){ 
  ta <- table(x)
  tam <- max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}
revol_util_missing <- Mode1(loan_dataset$revol_util)
 
# Imputing missing values in revol_util with Mode of the column '0'

loan_dataset$revol_util[is.na(loan_dataset$revol_util)] <- revol_util_missing

# Moving on to employee length, first finding count & percent of "n/a" values in emp_length
summary(loan_dataset$emp_length)
sum(loan_dataset$emp_length == "n/a")
percemt_emp_length_miss <- (sum(loan_dataset$emp_length == "n/a")/39717)*100

# 2.7 % of observations do not have any employment tenure data, however this does not have any impact on the 
#Analysis objective, hence leaving them as it is.


#Imputing emp_title, desc & title column blank values with "Unknown"

loan_dataset$emp_title[loan_dataset$emp_title == ""] <- "unknown"
loan_dataset$desc[loan_dataset$desc == ""] <- "unknown"
loan_dataset$title[loan_dataset$title == ""] <- "unknown"

#Converting some important factor values to Upper case
loan_dataset$emp_title <- toupper(loan_dataset$emp_title)
loan_dataset$home_ownership <- toupper(loan_dataset$home_ownership)
loan_dataset$verification_status <- toupper(loan_dataset$verification_status)
loan_dataset$loan_status<- toupper(loan_dataset$loan_status)
loan_dataset$purpose<- toupper(loan_dataset$purpose)

#Rounding up some columns to upto two decimals
summary(loan_dataset)
loan_dataset$delinq_2yrs <- round(loan_dataset$delinq_2yrs, digits = 2)
loan_dataset$funded_amnt_inv <- round(loan_dataset$funded_amnt_inv, digits = 2)
loan_dataset$total_pymnt <- round(loan_dataset$total_pymnt, digits = 2)
loan_dataset$total_rec_late_fee<- round(loan_dataset$total_rec_late_fee, digits = 2)
loan_dataset$collection_recovery_fee<- round(loan_dataset$collection_recovery_fee, digits = 2)
loan_dataset$recoveries<- round(loan_dataset$recoveries, digits = 2)

#converting issue_d, into correct date format.

loan_dataset$issue_d <- paste("01", loan_dataset$issue_d, sep = "-")
loan_dataset$issue_d <- as.Date(loan_dataset$issue_d , format = "%d-%b-%y")

#Seperating Month and Year and adding as columns

loan_dataset$issue_d_month <- format(loan_dataset$issue_d, "%b")
loan_dataset$issue_d_year <- format(loan_dataset$issue_d, "%Y")
loan_dataset$issue_d<- format(loan_dataset$issue_d, "%b-%Y")


#reordering the dataset
colnames(loan_dataset)
loan_dataset <- loan_dataset[ , c(1:16,49,50,17:48)]
summary(loan_dataset)     


#deriving debt income ratio ( ratio of debt and income ) as this is one of driving factor for loan defatult
loan_dataset$debt_income_ratio <- round((loan_dataset$loan_amnt / loan_dataset$annual_inc) * 100)

write.csv(loan_dataset, "loan_cleaned.csv", row.names = FALSE)


#Data Cleaning Activity can be closed here at this point and we will now proceed with Univariate Analysis (Analysing individual components)

################ DATA CLEANING CLOSURE ################


################ UNI-VARIATE & Bi-Variate ANALYSIS ################

library(ggplot2)

#Plotting Loan status v/s Frequency of loans
ggplot(loan_dataset,aes(x=loan_dataset$loan_status))+geom_bar()+labs(x="Status", y="loan count")

#plotting driving factors for charged off loan as this is the objective of case study
#loan status and tenure : tenure of 36 months have the higher chances of paying

#plotting relative chart : 60 month have  higher chances of not paying loan
ggplot(loan_dataset, aes(term, group = loan_status)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +   ylab("relative frequencies")  + theme(legend.position='none') +
  facet_grid(~loan_status)

#loan status and home ownership: ''own'have less cases for charged off 
ggplot(loan_dataset,aes(x=loan_dataset$loan_status,fill=loan_dataset$home_ownership))+geom_bar(stat='count',position="dodge")+ggtitle("loan status vs homeownership")+
  labs(x="Status", y="loan count") +labs(fill="home ownership")

#plotting relative graph for loan status and home ownership : rent cases have more chances for charged off
ggplot(loan_dataset, aes(home_ownership, group = loan_status)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) + theme(legend.position='none') +
  ylab("relative frequencies") +
  facet_grid(~loan_status) 

#loan status and emp length: 10+ yrs have higher chances of charged of cases
ggplot(loan_dataset, aes(emp_length, group = loan_status)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+ theme(legend.position='none') +
  facet_grid(~loan_status) 

ggplot(loan_dataset, aes(x = emp_length, col = loan_status)) + geom_bar(position = "fill")


#loan status and verification_status: not verified have higher % of charged off
ggplot(loan_dataset,aes(x=loan_dataset$issue_d_year,fill=loan_dataset$verification_status, position = "fill"))+geom_bar(stat='count',position="dodge")+ggtitle("verification_status by year vs Number of loan requests")+
  labs(x="Status", y="loan count") +labs(fill="verification_status")


#loan status and verification_status: not verified have higher % of charged off
ggplot(loan_dataset,aes(x=loan_dataset$loan_status,fill=loan_dataset$verification_status))+geom_bar(stat='count',position="dodge")+ggtitle("loan status vs verification_status")+
  labs(x="Status", y="loan count") +labs(fill="verification_status")

#relative plot for loan status and verification_status
ggplot(loan_dataset, aes(verification_status, group = loan_status)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+ theme(legend.position='none') +
  facet_grid(~loan_status) 

# plot for address state and loan status : CA has higher cases of charged off
ggplot(loan_dataset,aes(x=addr_state ,fill=loan_status))+geom_bar(stat='count')+ggtitle("loan status vs State")+
  labs(x="state", y="loan count") +labs(fill="loan_status")

#but NE has higher probability for charged off ; though the loan count is minimal
ggplot(loan_dataset, aes(x = addr_state, col = loan_status)) + geom_bar(position = "fill")

# plot for purpose and loan status : debt consolidation have higher cases of charged off
ggplot(loan_dataset, aes(x = purpose, col = loan_status)) + geom_bar(position="dodge")

ggplot(loan_dataset, aes(purpose, group = loan_status)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+ theme(legend.position='none')+
  facet_grid(~loan_status) 

#but the higher probability is for small business though the loan count is less
ggplot(loan_dataset, aes(x = purpose, col = loan_status)) + geom_bar(position = "fill")+theme(axis.text.x = element_text(angle = 30, vjust = 0.5))


# plot for grade and loan status : grade A have higher probibilty of payment ; Grade B , C have less
#probability for payment 
ggplot(loan_dataset, aes(grade, group = loan_status)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +theme(axis.text.x = element_text(angle = 0, vjust = 0.5))+  theme(legend.position='none')+ 
  facet_grid(~loan_status) 


#clearly the lower the grade the lower the probability of full paid
ggplot(loan_dataset, aes(x = grade, col = loan_status)) + geom_bar(position = "fill")


#Plotting Correlation between all numeric data & drawing insights to create relevant plots in Tableau

loan_numeric <- select_if(loan_dataset, is.numeric)
loan_corr <- round(cor(loan_numeric),2)
#install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(loan_corr)

#Debt to income ratio by year & Loan Status shows an upward trend
y_axis <- aggregate(debt_income_ratio~issue_d_year,loan_dataset, length)

ggplot(y_axis,aes(issue_d_year,debt_income_ratio,group = 1))+geom_line()+ggtitle("Understanding - Debt to income Ratio")+
  labs(x="Year of loan issue", y="Debt Income ratio")


#doing the box plot to find the relation b/w loan status and loan amount: cannot draw any inference
ggplot(loan_dataset, mapping = aes(y = loan_amnt, x =loan_status))+geom_boxplot()+labs(x="status", y="loan amount")


#boxplot debt_income_ratio and loan status : debt_income_ratio is higher for charged of cases than full paid
ggplot(loan_dataset, mapping = aes(y = debt_income_ratio, x =loan_status))+geom_boxplot()+labs(x="status", y="Debt income ratio")


#boxplot dti and loan status : median dti is higher for charged of cases than fully paid
ggplot(loan_dataset, mapping = aes(y = dti, x =loan_status))+geom_boxplot()+labs(x="status", y="dti")

#boxplot int_rate and loan status : median int_rate is higher for charged of cases than fully paid
ggplot(loan_dataset, mapping = aes(y = int_rate, x =loan_status))+geom_boxplot()+labs(x="interest rate", y="Debt income ratio")




