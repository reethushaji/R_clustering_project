#This dataset contains 29 variables and 2240 observations about different customers.

#People
#ID: Customer's unique identifier
#Year_Birth: Customer's birth year
#Education: Customer's education level
#Marital_Status: Customer's marital status
#Income: Customer's yearly household income
#Kidhome: Number of children in customer's household
#Teenhome: Number of teenagers in customer's household
#Dt_Customer: Date of customer's enrollment with the company
#Recency: Number of days since customer's last purchase
#Complain: 1 if customer complained in the last 2 years, 0 otherwise

#Products
#MntWines: Amount spent on wine in last 2 years
#MntFruits: Amount spent on fruits in last 2 years
#MntMeatProducts: Amount spent on meat in last 2 years
#MntFishProducts: Amount spent on fish in last 2 years
#MntSweetProducts: Amount spent on sweets in last 2 years
#MntGoldProds: Amount spent on gold in last 2 years

#Promotion
#NumDealsPurchases: Number of purchases made with a discount
#AcceptedCmp1: 1 if customer accepted the offer in the 1st campaign, 0 otherwise
#AcceptedCmp2: 1 if customer accepted the offer in the 2nd campaign, 0 otherwise
#AcceptedCmp3: 1 if customer accepted the offer in the 3rd campaign, 0 otherwise
#AcceptedCmp4: 1 if customer accepted the offer in the 4th campaign, 0 otherwise
#AcceptedCmp5: 1 if customer accepted the offer in the 5th campaign, 0 otherwise
#Response: 1 if customer accepted the offer in the last campaign, 0 otherwise

#Place
#NumWebPurchases: Number of purchases made through the company’s web site
#NumCatalogPurchases: Number of purchases made using a catalogue
#NumStorePurchases: Number of purchases made directly in stores
#NumWebVisitsMonth: Number of visits to company’s web site in the last month




















library(cluster)
library(tidyverse)
library(Rtsne)
library(factoextra)
library(lares)
df<- read.csv("C:/Users/hp/Downloads/customer_analysis_R/customer_behaviour.csv")
dim(df)
View(df)

#HANDLING MISSING DATA
#----------------------

# Check missing information:
colSums(is.na(df))
#Remove records with missing income:
df<-na.omit(df)
colSums(is.na(df))
dim(df)



#FEATURE ENGINEERING
#--------------------

#adding new features
---------------------
  #1.customer age from the birth year
  df['Age']= 2021-df$Year_Birth

#2.no of children for the customers
df['Child']=df$Kidhome+df$Teenhome


#3. total amount spent on various items
df['Spent'] = df$MntWines+df$MntFruits+df$MntMeatProducts+df$MntFishProducts+ 
  df$MntSweetProducts+df$MntGoldProds

#4. feature indicating parenthood
df['Is_parent']= ifelse(df$Child >0, 1, 0)
View(df)

#5. Details about previous campaigns

df['accepted_campaigns']=df$AcceptedCmp1+df$AcceptedCmp2+df$AcceptedCmp3+
  df$AcceptedCmp4+df$AcceptedCmp5

#data transformation/value replacement
--------------------------------------
  
 #marital status
unique(df$Marital_Status)
df<- df%>% mutate(Marital_Status = replace(Marital_Status, Marital_Status == "YOLO" |
                                             Marital_Status == "Absurd" | Marital_Status == "Alone"|
                                             Marital_Status == "Divorced" | Marital_Status == "Widow", "Single"))
df<-df %>% mutate(Marital_Status = replace(Marital_Status, Marital_Status == "Together" |
                                             Marital_Status == "Married", "Couple"))
unique(df$Marital_Status)

#education

unique(df$Education)
df <- df %>% mutate(Education = replace(Education, Education == "Graduation" | Education == "PhD" |
                                          Education == "Master", "Graduate"))
df<- df %>% mutate(Education = replace(Education, Education == "Basic" | 
                                         Education == "2n Cycle", "non-Graduate"))

unique(df$Education)



#DIMENSIONALITY REDUCTION
----------------------------
#feature selection/removal
View(df) 
names(df)
df=df[c(-1,-2,-6,-7,-8,-21,-22,-23,-24,-25,-27,-28)]
names(df)
dim(df)

            # categorical variables to factor


            #edu_factor <- factor(df$Education)
            #levels(edu_factor)
            #Maritalsts_factor <- factor(df$Marital_Status)
            #levels(Maritalsts_factor)
            #3complain_factor <- factor(df$Complain)
            #levels(complain_factor)
            ##isparent_factor <- factor(df$Is_parent)
            ###levels(isparent_factor)


#EDA
----
  
  #exploring the unique values in the categorical features
  
  # Count values in Category1 and create a summary dataframe
  count_marital_status<- df %>% 
  group_by(Marital_Status) %>% 
  summarise(count = n())
# Count values in Category2 and create a summary dataframe
count_education<- df %>% 
  group_by(Education) %>% 
  summarise(count = n())
# Print the summary data frames
print(count_marital_status)
print(count_education)



summary(df)

# OUTLIER DETECTION AND REMOVAL
-------------------------------
#income
ggplot(df) + geom_boxplot(aes(Income))
df<- df %>% filter(Income < 600000)

#age
ggplot(df) + geom_boxplot(aes(Age))
df<- df %>% filter(Age < 100)

summary(df$Age)
summary(df$Income)


#VISUALIZATIONS
---------------
#bar chart-income
ggplot(df) + geom_histogram(aes(Income), fill = "violet",color = "darkgreen")

#bar chart-age
ggplot(df) + geom_histogram(aes(Age), fill = "darkgreen",color="black")

#box plot-income
boxplot(df$Income,main = "customer income",xlab = "Income",ylab = "",
        col = "#adcae6",border = "blue",horizontal = TRUE,notch = TRUE)

#scatter plot income-total spent
ggplot(df,aes(x=Income,y=Spent))+geom_point(col="turquoise")

#histogram - Total spent
hist(df$Spent,50,xlab = "Total Spent",col="#adcae6")

#box plot   Education-Spent
ggplot(df, aes(x=Education,y=Spent,fill=Education))+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=T)

#box plot   Marital Status-Spent
ggplot(df, aes(x=Marital_Status,y=Spent,fill=Marital_Status))+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=T)



#The variables that correlate the most with Income:
corr_var(df, Income, top = 12)

#The variables that correlate the most with Spent:
corr_var(df, Spent, top = 12)

#Do older people complain more?

ggplot(df) + geom_boxplot(aes(factor(Complain),Age))

#Do richer people complain more?

ggplot(df) + geom_boxplot(aes(factor(Complain), Income))

#Do people with children complain more?

ggplot(df) + geom_boxplot(aes(factor(Complain),Child))


  
#STANDARDIZATION
---------------
#Scaling
View(df)
columns_to_scale<- c("Income","Recency","MntWines","MntFruits","MntMeatProducts","MntFishProducts","MntSweetProducts","MntGoldProds","NumDealsPurchases","NumWebPurchases",   
 "NumCatalogPurchases","NumStorePurchases","NumWebVisitsMonth","Response","Age" ,               
 "Child","Spent","accepted_campaigns","Is_parent","Complain")

df[, columns_to_scale] <- scale(df[, columns_to_scale])
View(df)

               #df<-scale(df[,3:22])


#MODEL BUILDING
----------------
  #KMEANS 
  --------

  
sapply(df,class)
# Perform label encoding on the 'Category' column
#(Assign unique integer codes to categorical columns)

categorical_columns <- c("Education","Marital_Status")

for (col in categorical_columns) {
  df[[col]] <- as.numeric(factor(df[[col]], levels = unique(df[[col]])))
}
# View the modified data frame
View(df)

  
#finding the optimal number of clusters
#elbow method:

clust<-fviz_nbclust(df,kmeans,method="wss")+geom_vline(xintercept=3,linetype=2)
clust

     #This method suggests 3 

set.seed(123)
km.res <- kmeans(df, 3, nstart = 10)
print(km.res$centers)
print(km.res$size)
print(km.res$betweenss/km.res$totss)
fviz_cluster(km.res, df, geom = "point",ellipse.type = "norm",repel = TRUE)
df['cluster']=as.factor(km.res$cluster)
View(df)

count_clusters_no<- df %>% 
  group_by(cluster) %>% 
  summarise(count = n())
count_clusters_no

attach(df)

#cluster-Spent
spentplot = ggplot(df, aes(x=cluster,y=Spent,fill=cluster))+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=T)
spentplot

ggplot(df, aes(Spent)) + 
  geom_histogram(aes(fill = factor(cluster)), color = "white") + 
  theme(legend.position = "bottom") + labs(fill = "Cluster")

#cluster-Age
ageplot = ggplot(df, aes(x=cluster,y=Age,fill=cluster))+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=T)
ageplot

ggplot(df, aes(Age)) + 
  geom_histogram(aes(fill = factor(cluster)), color = "white") + 
  theme(legend.position = "bottom") + labs(fill = "Cluster")


#cluster-income
incomeplot = ggplot(df, aes(x=cluster,y=Income,fill=cluster))+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=T)
incomeplot


ggplot(df, aes(Income)) + 
  geom_histogram(aes(fill = factor(cluster)), color = "white") + 
  theme(legend.position = "bottom") + labs(fill = "Cluster")

#cluster-web purchases
wpplot = ggplot(df, aes(x=cluster,y=NumWebPurchases,fill=cluster))+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=T)
wpplot

#cluster-catalog purchases
cpplot = ggplot(df, aes(x=cluster,y=NumCatalogPurchases,fill=cluster))+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=T)
cpplot

#cluster-store purchases
spplot = ggplot(df, aes(x=cluster,y=NumStorePurchases,fill=cluster))+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=T)
spplot

#cluster-income vs spent
ggplot(df) + 
  geom_point(aes(Income,Spent, color = factor(cluster)))

#marital status of people in each cluster:

  ggplot(df,aes(x=Marital_Status,fill=factor(cluster)))+geom_bar(position = "dodge")


#education of people in each cluster:

ggplot(df,aes(x=Education,fill=factor(cluster)))+geom_bar(position = "dodge")

#Parent/not in each cluster:

ggplot(df,aes(x=Is_parent,fill=factor(cluster)))+geom_bar(position = "dodge")

#Children and cluster
ggplot(df) + 
  geom_bar(aes(cluster, fill = factor(Child))) + 
  labs(fill = "children")

corr_var(df, cluster)






