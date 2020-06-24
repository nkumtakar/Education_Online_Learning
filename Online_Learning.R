####-------------------------STAT642 FINAL PROJECT----------------------------------------------####


        #clean workplace
        rm(list=ls())
        
        
        # setting working directory
        setwd("C:/Users/neelk/OneDrive/Documents/Drexel/Classes/Spring 2020 Quarter/Datamining for Buisness Analytics/Final Project")
        
        #read data
        df <-read.csv("Project_Data.csv")
        
        #Check data
        head(df)
        str(df)
      

####-------------------------DATA PREPROCESSING----------------------------------------------------------####

        ## For depedent variable - final_result
        
        # Delete final_result-withdrawn
        df <-subset(df, final_result=="Distinction"|final_result=="Fail"|final_result=="Pass")
        # Combine "Distinction" with "Pass"
        levels(df$final_result) <- list("Pass"=c("Distinction","Pass"), "Fail"=c("Fail"))
        
        
        ## For indepedent variable - code_module
        #only extract "STEM" , remove "Social Science"
        df <-subset(df, code_module=="STEM")
        df <-df[,-2] # delete variable - code_module, as all obs in "STEM" code, we don't need to keep this variable
        
        
        ## For indepedent variable - code_presentation
        levels(df$code_presentation) <- list("2014B-STEM"=c("2014B"), "2014J-STEM"=c("2014J"))
        
        
        ## For indepedent variable - highest_education
        levels(df$highest_education) <- list("Lower Than A Level"=c("Lower Than A Level"), 
                                             "A Level or Equivalent"=c("A Level or Equivalent"),
                                             "Higher Than A Level"=c("HE Qualification","Post Graduate Qualification"),
                                             "No Formal Education Qualification"=c("No Formal quals")) 
        
        
        
        ## For indepedent variable - sum_click 
        # sum 5 variabls about "click" together
        df$Sum_clicks <- apply(df[,15:19], 1, sum)
        df <- df[,-c(15:19)]
        
        
        ## Adjust other variables
        df <-df[,-c(7,8,10,21,22,24,26)]
        
        
        ## Adjust variables sequence
        library(dplyr)
        df <- select(df,-final_result,everything())
        
        ## Change Ag_band levels
        
        levels(df$age_band) = c("< 35", "35-55", "> 55")

        
####---------------------------Data Exploration-------------------------------------------------------------#### 
        
        library(ggplot2)
        library(ggpubr)
        library(ggplot2)   #for plot
        library(reshape)   #for melt
        library(gridExtra) #plotting
        library(grid)
        library(lattice)
        
        
        
        #----------------------------------Explore Categorical variables--------------------------------------#
        
        #1.The depedent variable - final results 
        table(df$final_result)
        
        ggplot(data=df,aes(x=factor(final_result),fill=final_result))+geom_bar(position="dodge",width=0.7)+
          scale_fill_manual(values=c("lightsteelblue","orange"),  
                            name="Final Results",
                            labels=c("Pass", "Fail"))+
          ggtitle("Final Results Distribution") +
          xlab("") + ylab("Counts")+
          theme(
            plot.title = element_text(color="black", size=12, face="bold",hjust = 0.5),
            axis.title.y = element_text(color="black", size=12, face="bold"))
        
        
        
        
        ##Relationship between major "independent categorical variables" and the "depedent variable - final results"
        #par(mfrow=c(3,1))
        #2. age_band & final  result
        
        df$age_band = factor(df$age_band,levels(df$age_band)[c(1,3,2)]) # adjust factor levels sequence
        
        counts <- table(df$age_band, df$final_result)  
        counts
        
        fg1 <-ggplot(data=df,aes(x=factor(final_result),fill=factor(age_band)))+geom_bar(position="dodge")+
          scale_fill_manual(values=c("paleturquoise3","rosybrown3","peru"),  
                            name="Age",
                            labels=c("<35","35-55",">55"))+
          ggtitle("Figure.1  Age Distribution by Final Result") +
          xlab("Final Result") + ylab("Counts")+
          theme(
            plot.title = element_text(color="black", size=12, face="bold",hjust=0.5),
            axis.title.x = element_text(color="black", size=12, face="bold"),
            axis.title.y = element_text(color="black", size=12, face="bold"))
        fg1
        
        
        
        
        #3. highest_education & final  result
        
        df$highest_education = factor(df$highest_education,levels(df$highest_education)[c(3,1,2,4)])
        
        counts <- table(df$highest_education, df$final_result)  
        counts
        
        fg2 <-ggplot(data=df,aes(x=factor(final_result),fill=factor(highest_education)))+geom_bar(position="dodge",width = 0.9)+
          scale_fill_manual(values=c("burlywood1","gold2","goldenrod3","gold4"),  
                            name="Highest Education",
                            labels=c("Lower Than A Level","A Level or Equivalent", "Higher Than A Level",
                                     "No Formal Education Qualification"))+
          ggtitle("Figure.2  Highest Education Distribution by Final Result") +
          xlab("Final Result") + ylab("Counts")+
          theme(
            plot.title = element_text(color="black", size=12, face="bold",hjust = 0.5),
            axis.title.x = element_text(color="black", size=12, face="bold"),
            axis.title.y = element_text(color="black", size=12, face="bold"))
        fg2
        
        
        
        #4. imd_band & final  result
        df$imd_band = factor(df$imd_band,levels(df$imd_band)[c(2,3,1)])
        
        counts <- table(df$imd_band, df$final_result)  
        counts
        
        
        fg3 <- ggplot(data=df,aes(x=factor(final_result),fill=factor(imd_band)))+geom_bar(position="dodge",width = 0.6)+
          scale_fill_manual(values=c("lightsalmon","mediumpurple1","magenta1"),  
                            name="IMD Band",
                            labels=c("Low(0-30%)","Middle(30-60%)", "High(60-100%)"))+
          ggtitle("Figure.3  IMD Band Distribution by Final Result") +
          xlab("Final Result") + ylab("Counts")+
          theme(
            plot.title = element_text(color="black", size=12, face="bold",hjust = 0.5),
            axis.title.x = element_text(color="black", size=12, face="bold"),
            axis.title.y = element_text(color="black", size=12, face="bold"))
        fg3
        
        
        
        
        #----------------------------------Explore Numerical variables--------------------------------------#
        
        #1. Sum_click & final result
        
        fg4 <-ggplot(df,aes(x=factor(final_result),y=log10(Sum_clicks),color=final_result))+geom_boxplot()+
          ggtitle("Figure.4  Log of Clicktimes between Pass and Fail") +
          xlab("Final Result") + ylab("Log(Clicktimes)")+
          theme(
            plot.title = element_text(color="black", size=12, face="bold",hjust = 0.5),
            axis.title.x = element_text(color="black", size=12, face="bold"),
            axis.title.y = element_text(color="black", size=12, face="bold"))
        fg4
        
        
        #2. Activity "homepage, oucontent, subpage, resource, ouwiki, dataplus,quiz,  page,glossary" 
        df_stem_activity <- df[,c(11:19)]  
        
        fg5 <-data_melt<-melt(df_stem_activity)
        ggplot(data_melt,aes(x=variable,y=log10(value),color=variable))+geom_violin()+ 
          ggtitle("Figure.5  STEM Activity Types") +
          xlab("") + ylab("Log(Activity Type Clicktimes)")+
          theme(
            plot.title = element_text(color="black", size=10, face="bold",hjust = 0.5),
            axis.title.x = element_text(color="black", size=10, face="bold"),
            axis.title.y = element_text(color="black", size=10, face="bold"))
        fg5
        
        

        plot2= ggplot(df, aes(TMA_score, CMA_score, color = highest_education, shape=final_result)) + geom_point() + ggtitle("TMA vs CMA")
        
        
        plot8= ggplot(df, aes(x=TMA_score, y= Exam_score,  color=final_result)) + geom_point() + ggtitle("TMA vs Exam Score")
         
      
        
        plot10=ggplot(df, aes(TMA_score, fill= disability)) + geom_histogram(binwidth = 15) + ggtitle('Tutored Marked Assessment on Disability')
        
        Bivariate_plots = ggarrange(plot8, plot10, plot2)
        
        Bivariate_plots

  
  ####---------------------------UNSUPERVISED LEARNING-------------------------------------------------------------####        
  
  ####---------------------------HIERARCHIAL CLUSTERING-------------------------------------------------------------####   
         
        
        
     
        
        
        
  ####--------------------------K-MEANS CLUSTERING-------------------------------------------------------------#### 
        
        df_KM<-df[,-1]
        
        

        library(cluster) #clustering
        library(factoextra) #cluster validation
        library(fpc) #kmeans cluster plots
        library(gridExtra) #plotting
        library(caret) #machine learning (feature selection)
        
      
        #1. Scaled data
        df_KM$code_presentation <-as.numeric(df_KM$code_presentation)
        df_KM$gender <-as.numeric(df_KM$gender)
        df_KM$highest_education <-as.numeric(df_KM$highest_education)
        df_KM$imd_band <-as.numeric(df_KM$imd_band)
        df_KM$age_band <-as.numeric(df_KM$age_band)
        df_KM$disability <-as.numeric(df_KM$disability)
        
        df_scaled <- sapply(df_KM[,1:20], FUN = scale)
        
        
        #2. Cluster Scaled K-means, centers=2
        set.seed(831)
        kmeans2_s <- kmeans(df_scaled[,1:20], centers=2, 
                            trace=FALSE, nstart=30)
        kmeans2_s
        
        
        #3. create a distance matrix
        df_scaled_dist <- dist(df_scaled[,1:20])
        
        
        
        
        #----------------------------------2.2 Cluster External Validation--------------------------------------#
        # K-means (2 Clusters on the scaled data)
        table(final_result=df$final_result, Clusters=kmeans2_s$cluster)
        
        
        
        #----------------------------------2.3 Cluster Internal Validation--------------------------------------#
        ##A. Total Within-cluster sum of squares  (small preferred)
        kmeans2_s$tot.withinss
        
        
        
        
        ##B. Silhouette
        # Values close to 1 suggest that the observation is well matched to the 
        # assigned cluster
        # Values close to 0 suggest that the observation is borderline matched 
        # between two clusters
        # Values close to -1 suggest that the observations may be assigned to 
        # the wrong cluster
        plot(silhouette(x=kmeans2_s$cluster, dist=df_scaled_dist), main="") 
        
        
        
        
        ##C.Alternative Measures of Validity
        
        # average.between: large average distance between clusters preferred
        # average.within : small average distance within clusters preferred
        # Dunn Index (dunn): computed as minimum separation/maximum 
        #                    diameter, higher values are preferred
        
        cluster.stats(d=df_scaled_dist, clustering=kmeans2_s$cluster)
        
        
  ###  ------------------------------------SUPERVISED LEARNING---------------------------------------------#####  
        
      
####-----------------------------------K Nearest Neighbors (KNN)------------------------------------------------####
       
        library(class) 
        library(caret)
        library(dplyr)
        
        
        df_KNN = df[, -1]
        
        
        df_KNN$code_presentation_M = ifelse(df_KNN$code_presentation == "2014B-STEM", 1, 0)
        df_KNN$code_presentation_F = ifelse(df_KNN$code_presentation == "2014B-STEM", 1, 0)
        
    
        df_KNN$gender_M = ifelse(df_KNN$code_presentation == "M", 1, 0)
        df_KNN$gender_F = ifelse(df_KNN$code_presentation == "F", 1, 0)
        
        df_KNN$highest_education_LowerThanALevel = ifelse(df_KNN$highest_education== "Lower Than A Level", 1, 0)
        df_KNN$highest_education_ALevel = ifelse(df_KNN$highest_education == "A Level or Equivalent" , 1, 0)
        df_KNN$highest_education_HigherThanA = ifelse(df_KNN$highest_education == "Higher Than A Level" , 1, 0)
        df_KNN$highest_education_NoFormalQual = ifelse(df_KNN$highest_education == "No Formal Education Qualification" , 1, 0)
        
        
        df_KNN$imd_band_high = ifelse(df_KNN$imd_band == "High(60-100%)", 1, 0)
        df_KNN$imd_band_low = ifelse(df_KNN$imd_band == "Low(0-30%)", 1, 0)
        df_KNN$imd_band_medium = ifelse(df_KNN$imd_band == "Middle(30-60%)", 1, 0)
        
        df_KNN$age_band_LessThan35 = ifelse(df_KNN$age_band ==  "< 35", 1, 0)
        df_KNN$age_band_Between35And55 = ifelse(df_KNN$age_band ==   "35-55" , 1, 0)
        df_KNN$age_band_Over55 = ifelse(df_KNN$age_band ==  "> 55", 1, 0)
        
        df_KNN$disability_Y = ifelse(df_KNN$disability == "Y", 1, 0)
        df_KNN$disability_N = ifelse(df_KNN$disability == "N", 1, 0)
        
        
        df_KNN = subset(df_KNN, select = -c(code_presentation, gender, highest_education, imd_band, age_band, disability))
          
       
        
        
        ## Min-Max normalization - transform the dataset using min/max normalization, x= (x- min(x) / range(x)
        
        prepObj <- preProcess(x=df_KNN, method="range")
        
        
        df_KNN <- predict(prepObj, df_KNN)
        
        
        ## Split into Training and Testing
        
        set.seed(831)
        
        n <- createDataPartition(df_KNN$final_result, p=0.80, list=FALSE)
        
        
        df_KNN_train = df_KNN[n, ] 
        
        
        df_KNN_test = df_KNN[-n, ]
        
    
  
        
        ## Using a 'best guess' value of k
        # Let's find a good starting point for k
        
       
        k.choice <- ceiling(sqrt(nrow(df_KNN_train)))
        k.choice  ## k=74 was chosen
        
        set.seed(831)
        
        KNN_model <- knn(train=df_KNN_train[,-15],
                        test=df_KNN_test[,-15], 
                        cl=df_KNN_train$final_result, 
                        k=k.choice)
      
        
        KNN_model_Test_Results= confusionMatrix(data=KNN_model, reference=df_KNN_test$final_result,
                        mode="everything")  ## the model does well on the testing, 90% accuracy 
        
        KNN_model_Test_Results
        
  
       
       ####-------------------------Random Forest Model ----------------------------------------------------------####
       
   
       library(randomForest) # Random Forest
       library(caretEnsemble)
       library(caret)
       library(randomForestExplainer)
       
           ## Remove ID
           
           df_RF = df[, -1]
           
          
           set.seed(831)
           
           ## Split into training and testing, 80-20
           
           x<- createDataPartition(df_RF$final_result, p=.80, list=FALSE)
           
        
           
           df_RF_train =df_RF[x,]
           df_RF_test = df_RF[-x,]
           
        
    
           
       
           set.seed(831)
           
           
           RF_Model<- randomForest(final_result~.,
                                  data=df_RF_train,
                                  importance=TRUE, 
                                  proximity=TRUE, 
                                  ntree=500)
           RF_Model
           
           ## Predict on training and testing
           
           RF_model_Train = predict(RF_Model, df_RF_train[,-21])
           
           RF_model_Test = predict(RF_Model, df_RF_test[, -21])
           
           ## Compare performance against training and testing data
           
           RF_model_Train_Results = confusionMatrix(RF_model_Train, df_RF_train$final_result, mode="everything", positive = "Pass")
           
             
           RF_model_Test_Results= confusionMatrix(RF_model_Test, df_RF_test$final_result, mode="everything", positive="Pass")
           
           
           RF_model_Train_Results
           
           RF_model_Test_Results
      
           
           
           
####------------------------------------Decision Tree Model ----------------------------------------------------------####
           
           # Load libraries
           library(rpart) # decision trees
           library(rpart.plot) # decision tree plots
           library(caret) # machine learning
           library(ggplot2)
           
        
            df_DT = df[,-1]  ## remove ID
           
         
           
           
          ## Split data into training and testing, 80-20
            
           set.seed(831)
           samp <- createDataPartition(df_DT$final_result, p=.80, list=FALSE)
           train = df_DT[samp, ] 
           test = df_DT[-samp, ]
           
           
           ## Basic Classification Tree Model (no hyperparameter tuning)
           # We use the rpart() function in the rpart package to
           # perform DT classification to our training dataset.
           # Note: rpart defaults to using Gini for split decisions but
           # can change to using entropy by specifying split="information"
           # in parms=list().
          
           
           set.seed(831)
           
           dt.rpart <- rpart(formula = final_result ~ ., 
                             data = train, 
                             method = "class")
           
        
           
    
            ## Predict on training and testing data
           
           tree.is.preds = predict(object=dt.rpart,
                                   newdata=train, type = "class")
           
           tree.out.preds= predict(object=dt.rpart,
                                   newdata=test, type = "class")
           
           
           
          ## Compare Testing and Training performances of the model
           
           DT_Results_Train=confusionMatrix(data=tree.is.preds, 
                                            reference=train$final_result, mode="everything")
           
           DT_Results_Test= confusionMatrix(data=tree.out.preds, 
                                            reference=test$final_result, mode="everything")
           DT_Results_Train
           DT_Results_Test
           
           
####------------------------------------------------------Neural Network------------------------------------------------------------------####
           
           
           library(nnet) # neural networks
           library(caret)
           
        
           df_ANN = df[,-1]
           
           ## Convert all factor variables into dummy variables as Ann does not handle these types of variables
           
           df_ANN$code_presentation_M = ifelse(df_ANN$code_presentation == "2014B-STEM", 1, 0)
           df_ANN$code_presentation_F = ifelse(df_ANN$code_presentation == "2014B-STEM", 1, 0)
           
           
           df_ANN$gender_M = ifelse(df_ANN$code_presentation == "M", 1, 0)
           df_ANN$gender_F = ifelse(df_ANN$code_presentation == "F", 1, 0)
           
           df_ANN$highest_education_LowerThanALevel = ifelse(df_ANN$highest_education== "Lower Than A Level", 1, 0)
           df_ANN$highest_education_ALevel = ifelse(df_ANN$highest_education == "A Level or Equivalent" , 1, 0)
           df_ANN$highest_education_HigherThanA = ifelse(df_ANN$highest_education == "Higher Than A Level" , 1, 0)
           df_ANN$highest_education_NoFormalQual = ifelse(df_ANN$highest_education == "No Formal Education Qualification" , 1, 0)
           
           
           df_ANN$imd_band_high = ifelse(df_ANN$imd_band == "High(60-100%)", 1, 0)
           df_ANN$imd_band_low = ifelse(df_ANN$imd_band == "Low(0-30%)", 1, 0)
           df_ANN$imd_band_medium = ifelse(df_ANN$imd_band == "Middle(30-60%)", 1, 0)
           
           df_ANN$age_band_LessThan35 = ifelse(df_ANN$age_band ==  "< 35", 1, 0)
           df_ANN$age_band_Between35And55 = ifelse(df_ANN$age_band ==   "35-55" , 1, 0)
           df_ANN$age_band_Over55 = ifelse(df_ANN$age_band ==  "> 55", 1, 0)
           
           df_ANN$disability_Y = ifelse(df_ANN$disability == "Y", 1, 0)
           df_ANN$disability_N = ifelse(df_ANN$disability == "N", 1, 0)
           
           df_ANN = subset(df_ANN, select = -c(code_presentation, gender, highest_education, imd_band, age_band, disability))
           
        
           ## Rescaling via Range Method 
           
           b<- preProcess(df_ANN, method="range")
           
           df_ANN <- predict(b, df_ANN)
           
           set.seed(831)
           samp <- createDataPartition(df_ANN$final_result, p=.80, list=FALSE)
           ann.train = df_ANN[samp, ] 
           ann.test = df_ANN[-samp, ]
           
           ## Basic Model Building using the nnet Package
           
           # size sets the number of nodes in the hidden layer
  
           
           set.seed(831)
           
           nnmod <- nnet(final_result~., data=ann.train, size=3, trace=FALSE)
           
           set.seed(831)
           
           ## Predict on Training and Testing data S
           
           ANN_train <- predict(nnmod, 
                                ann.train[, -15],
                                type="class")
           
            ANN_test   <- predict(nnmod, 
                                 ann.test[, -15],
                                 type="class")
           
           ANN_Train_Results = confusionMatrix(factor(ANN_train ), train$final_result, mode= "everything")
           ANN_Test_Results = confusionMatrix(factor(ANN_test), test$final_result, mode="everything")
       
           
           ANN_Train_Results
           ANN_Test_Results
          
  ####-------------------------------------------------------NAIVE BAYES------------------------------------------------####
           
        
           ## Load libraries
           library(e1071) #naive bayes
           library(caret) #pre-processing, machine learning
           
         
           
          df_NB= df[, -1]
           

           
           ## Data Transformation
           # Since we have numerical inputs, the data is assumed to be Gaussian.
           # We can use the preProcess() function on the data and specify
           # method="BoxCox", which will attempt to convert non-normal data
           # to a normal (Gaussian) distribution. Alternatively, if we want to 
           # transform data before analysis using preProcess() for Z-Score 
           # transformation, we can use method=c("center","scale") and for 
           # min-max we can use method="range". These can also be specified
           # during training using hyperparameter tuning using the preProcess 
           # argument of the train() function in the caret() package.
           
           # Note: The function will be applied to numerical variables and
           # will ignore any factor variables
           
           prepObj_NB <- preProcess( df_NB, method="BoxCox")
           
           # Then, we would use the predict() function to create a new
           # dataframe containing our normally distributed data
           
           df_NB <- predict(prepObj_NB,  df_NB)
           
          
        
           
           ## Training and Testing
           
  
           
           # Splitting the data into training and testing sets using an 
           # 80/20 split rule
           
           set.seed(831)
           
           sub <- createDataPartition(df_NB$final_result, p=0.80, list=FALSE)
           
          nb.train = df_NB[sub, ] 
          nb.test = df_NB[-sub, ]
           
          set.seed(831)
           
           nb_mod <- naiveBayes(formula=final_result~.,
                                data=nb.train)
           NB_train = predict(nb_mod, nb.train[,-21])
           
           NB_test = predict(nb_mod, nb.test[, -21])
           
           NB_train_results = confusionMatrix(NB_train, nb.train$final_result, mode="everything")
           
           NB_test_results = confusionMatrix(NB_test, nb.test$final_result, mode="everything")
           
           NB_train_results
           NB_test_results
          

           
 ####-------------------------------------MODEL IMPROVEMENT (Random Forest)------------------------------------------------####           
           
           ## We see that the Random Forest performs the best in the training set. But it seems to have overfit
           
           ## We will use feature selection, sampling and hyper-parmaeter testing in order to improve this model.
           
           
           ## Feature Selection 
           
         
           
           library(caret)
           
           library(randomForest) # Random Forest
           library(caretEnsemble)
           library(caret)
           library(randomForestExplainer)
           
           set.seed(831)
           
           rfe_DT <- rfeControl(functions = rfFuncs,
                                 method = "repeatedcv",
                                 number = 10,
                                 repeats = 3,
                                 verbose = FALSE)
           
           can_rfe <- rfe(x = df_RF[,-21], 
                          y = df_RF$final_result,
                          rfeControl = rfe_DT)
                
           
           can_rfe   ## Exam Score, TMA, Ouwiki, Sum_clicks and Quiz deemed the Top 5 important variables
           
           
           RF_ReducedSet = subset(df_RF, select = c( Exam_score,  TMA_score , ouwiki , Sum_clicks, quiz, final_result))
           
           set.seed(831)
           
           xx<- createDataPartition(RF_ReducedSet$final_result, p=.80, list=FALSE)
           
           RF_ReducedTrain= RF_ReducedSet[xx, ] 
           RF_ReducedTest= RF_ReducedSet[-xx, ]
           
           ## Next we need to undersample the training set
           
           set.seed(931)
           RF_ReducedTrain <- downSample(x=RF_ReducedTrain[, -6], 
                                  y=RF_ReducedTrain$final_result, 
                                  yname="final_result")
           
           
       
           
           varImpPlot(RF_Model, main = "Variable Importance of Random Forest Model")
           
           
           
           
           ## Hyperameter Tuning - Grid search 
           
           grids = expand.grid(mtry = seq(from = 1, to = 20, by = 2))
           
           set.seed(51)
           
           grid_ctrl <- trainControl(method = "repeatedcv",
                                     number = 10,
                                     repeats = 3,
                                     search="grid")
           
           set.seed(51)
           
           RF_HP_Model_Grid <- train(final_result~., 
                           data=RF_ReducedTrain, 
                           method="rf", 
                           trControl=grid_ctrl,
                           tuneGrid=grids)
           
           
           
           ## View the variable important of the RF. To do this, create the RF model using the training set that had
           ## the featured selected variables and
   
           set.seed(831)
           
          RF_Model_B<- randomForest(final_result~.,
                                  data=RF_ReducedTrain,
                                  importance=TRUE, 
                                  proximity=TRUE, 
                                  ntree=500)
         
          varImpPlot(RF_Model_B, main = "Variable importance of Random Forest")     ## Variable importance 
          
          plot_predict_interaction(RF_Model_B, RF_ReducedTrain, "Exam_score", "TMA_score")  ## variable interaction
          plot_predict_interaction(RF_Model_B, RF_ReducedTrain,"Exam_score", "Sum_clicks")
           
           
           
           ## Hyperparameter Tuning - Random Search
           
           
           set.seed(831)
           
           random_ctrl <- trainControl(method = "repeatedcv",
                                     number = 10,
                                     repeats = 3,
                                     search="random")
           
           set.seed(831)
           
           RF_HP_Model_Random <- train(final_result~., 
                                     data=RF_ReducedTrain, 
                                     method="rf", 
                                     trControl=random_ctrl)
           
           RF_HP_Model_Random_Train = predict(RF_HP_Model_Random, RF_ReducedTrain[, -6])
           
           RF_HP_Model_Grid_Train = predict(RF_HP_Model_Grid, RF_ReducedTrain[, -6])
           
           RF_HP_Model_Random_Test = predict(RF_HP_Model_Random, RF_ReducedTest[, -6])
           
           RF_HP_Model_Grid_Test = predict(RF_HP_Model_Grid, RF_ReducedTest[, -6])
                                     
           
           RF_Random_Train_Results= confusionMatrix(RF_HP_Model_Random_Train , RF_ReducedTrain$final_result, mode="everything")
           
           RF_Grid_Train_Results=confusionMatrix(RF_HP_Model_Grid_Train, RF_ReducedTrain$final_result, mode="everything")
           
          RF_Random_Test_Results= confusionMatrix(RF_HP_Model_Random_Test  , RF_ReducedTest$final_result, mode="everything")
           
          RF_Grid_Test_Results= confusionMatrix(RF_HP_Model_Grid_Test, RF_ReducedTest$final_result, mode="everything")
           
           
          RF_Random_Train_Results
           
          RF_Grid_Train_Results
          
          RF_Random_Test_Results
          
          RF_Grid_Test_Results
           

