/*GDS :
1.	It is short for Global Deficit Score.
2.	It is continuous outcome.
3.	It is called GDS_HNRC  in data set.
Response 2: 
DDS :
1.	It is short for Domain Deficit Score
2.	It is continuous outcome.
3.	There are 7 domains in total.
4.	DDS for each domain is called DDS_domian in data set.
Response 3:
GDS impairment score:
1.	It is called GDS_HNRC_impair in data set.
2.	It is a binary outcome.( 0 is no impairment, 1 is impairment).
Response 4: 
DDS impairment score:
1.	1. It is called DDS_domain_impair for each domain in data set.
2.	It is a binary outcome. ( 0 is no impairment, 1 is impairment).
VACS index 
1)	Age: continuous variable 
2)	Lower CD4: continuous variable 
3)	HIV -1 RNA: continuous variable 
4)	FIB-4: continuous variable : Calculated by three variables listed below 
 PLT COUNT: continuous variable
 ALT: continuous variable
 AST: continuous variable   
5)	Hep C con-infection: binary variable
6)	HGB: continuous variable
7)	GFR:  continous/catgorical variable 
*/

proc import datafile="C:\Users\sz86\Desktop\Christina\data\reloaded merge data set +\data set HIV +.csv" 
     out=HIVp
     dbms=csv
     replace;
     getnames=yes;
run;

proc print data=HIVp (obs=10);
title " data set obs=10";
run;

/*summary data set*/
proc means data=HIVp;
title " statsitics summary of vacsindex";
var vacsindex edu age vir_recv  cd4_rec HGB PLT ALT AST FIB;
class GDSimpair;
run;
proc freq data=HIVp;
title" statistics summary of GDSimpair and cocaine status";
table  COC*GDSimpair  hepc_mr*GDSimpair ;
run;

proc sort data=HIVp;
   by GDSimpair;
run;

ods select ssplots ;
proc univariate data =HIVp plot ;
title "statistic summary for continous variable";
var vacsindex edu age vir_recv  cd4_rec HGB PLT ALT AST FIB; 
by GDSimpair ;
run;

proc ttest data=HIVp sides=2 alpha=0.05 h0=0;
 	title "Two sample t-test vacs index";
 	class GDSimpair; 
	var vacsindex;
   run;

/*logistic regression for predicting different abilities(0/1) by VACS index */
ods  graphics on; 
goptions reset=all;
PROC GPLOT DATA=HIVp;
PLOT GDSimpair*vacsindex;
run;
proc logistic data=HIVp descending plots=effect;
title " logistic regression between GDSimpari and VACS index";
model GDSimpair = vacsindex/link=logit;
run;
proc logistic data=HIVp descending;
title " logistic regression between DDS_Attentionimpair and VACS index";
model DDS_Attention_impair = vacsindex;
run;
proc logistic data=HIVp descending;
title " logistic regression between DDS_ExecFxn_HNRC_impair  and VACS index";
model DDS_ExecFxn_HNRC_impair = vacsindex;
run;
proc logistic data=HIVp descending;
title " logistic regression between DDS_Fluency_impair  and VACS index";
model DDS_Fluency_impair = vacsindex;
run;
proc logistic data=HIVp descending;
title " logistic regression between DDS_InfoProc_impair  and VACS index";
model DDS_InfoProc_impair = vacsindex;
run;
proc logistic data=HIVp descending;
title " logistic regression between DDS_Learning_HNRC_impair and VACS index";
model DDS_Learning_HNRC_impair = vacsindex;
run;
proc logistic data=HIVp descending;
title " logistic regression between DDS_Memory_HNRC_impair and VACS index";
model DDS_Memory_HNRC_impair = vacsindex;
run;
proc logistic data=HIVp descending;
title " logistic regression between DDS_Motor_impair and VACS index";
model DDS_Motor_impair = vacsindex;
run;

/*univarite regression for predicting the score of different abilities by vacs index*/
proc reg data=HIVp;
title" univariate regression between GDS and VACS index";
model GDS =vacsindex;
run;
/*doing transformation*/
data HIVpc;
set  HIVp;
gds=GDS+0.001;/*since some GDS has 0 value*/
run;
proc transreg details data=HIVpc;
 model BoxCox(gds/ convenient lambda=-10 to 10 by 0.1) = identity(vacsindex);
 run;
data HIVpc;
set HIVpc;
gds1=(gds**0.3-1)/0.3;
run;
/*doing regression after transformation*/
proc reg data=HIVpc;
title" univariate regression between GDS transformed  and VACS index";
model gds1 =vacsindex;
run;
proc reg data=HIVp ;
title " univariate regression between DDS_Attentionand VACS index";
model DDS_Attention = vacsindex;
run;
proc reg data=HIVp ;
title " univariate regression between DDS_ExecFxn_HNRC  and VACS index";
model DDS_ExecFxn_HNRC = vacsindex;
run;
proc reg data=HIVp ;
title " univariate regression between DDS_Fluency  and VACS index";
model DDS_Fluency = vacsindex;
run;
proc reg data=HIVp ;
title " univariate  regression between DDS_InfoProc  and VACS index";
model DDS_InfoProc = vacsindex;
run;
proc reg data=HIVp ;
title " univariate regression between DDS_Learning_HNRC and VACS index";
model DDS_Learning_HNRC = vacsindex;
run;
proc reg data=HIVp;
title " univariate regression between DDS_Memory_HNRC and VACS index";
model DDS_Memory_HNRC  = vacsindex;
run;
proc reg  data=HIVp ;
title " univariate regression between DDS_Motor and VACS index";
model DDS_Motor = vacsindex;
run;

/* multivariate logistic regression */
/*1 stratify eGFR*/
data HIVpm;
set HIVp;
if GFR >= 60 then GFRc=0;
if GFR <=59 and GFR >=30 then GFRc=1;
if GFR <=29 and GFR >=15 then GFRc=2;
if GFR <15 then GFRc=3;
run;
proc print data=HIVpm(obs=10);
title" data set with eGFR coded";
run;
proc export data=HIVpm
   outfile='C:\Users\sz86\Desktop\Christina\data\HIV+withGFRcoding.csv'
   dbms=csv
   replace;
run;
proc freq data=HIVpm;
title" statistics summary of GDSimpair and cocaine status";
table  GFRc*GDSimpair  ;
run;
data HIVpm;
set HIVpm;
lvir_recv=log(vir_recv+0.001);
run;
proc print data=HIVpm(obs=10);
run;
proc logistic data=HIVpm descending ;
title" multivarite regression ";
class GFRc coc/ param=ref descending ;/*set GFRc as catogorical, and set it as dummy coding*/
model GDSimpair=age cd4_rec lvir_recv  hepc_mr HGB PLT ALT AST GFRc COC;
run;
PROC STANDARD DATA=HIVpm MEAN=0 STD=1 OUT=HIVpms;/*standardized varaibles*/
  VAR age cd4_rec lvir_recv  HGB PLT ALT AST;
RUN;
proc print data=HIVpms(obs=10);
run;
proc logistic data=HIVpms descending ;
title"  multivariate regression using standardized data";
class GFRc coc/ param=ref descending ;/*set GFRc as catogorical, and set it as dummy coding*/
model GDSimpair=age cd4_rec lvir_recv  hepc_mr HGB PLT ALT AST GFRc COC;
run;
proc logistic data=HIVpm descending ;
title" selec VCAS for GDS impair";
class GFRc coc/ param=ref descending ;/*set GFRc as catogorical, and set it as dummy coding*/
model GDSimpair=age cd4_rec vir_recv  hepc_mr HGB PLT ALT AST GFRc COC/ selection=backward;
run;
/*regression for GDS and all dependent variables.*/
proc sort data= HIVpm;
key GFRc/ descending;
run;
proc glm data=HIVpm order =data;
title" estimation and p-value for each predictor";
class GFRc coc;
model GDS=age cd4_rec vir_recv hepc_mr HGB PLT ALT AST GFRc COC/solution ss3;
run;
proc glmselect data=HIVpm;
title" select VCAS for GDS using original data";
class GFRc coc;
model GDS=age cd4_rec vir_recv hepc_mr HGB PLT ALT AST GFRc  COC;
run;

/*transformation for gds*/
data HIVpmc;
set  HIVpm;
gds=GDS+0.001;/*since some GDS has 0 value*/
run;
proc transreg details data=HIVpmc;
 model BoxCox(gds/ convenient lambda=-10 to 10 by 0.1) = identity(age cd4_rec vir_recv hepc_mr HGB PLT ALT AST GFRc  COC);
 run;
data HIVpmc;
set HIVpmc;
gds1=(gds**0.4-1)/0.4;
run;
proc reg data=HIVpmc;
model gds1=age cd4_rec vir_recv hepc_mr HGB PLT ALT AST GFRc  COC/selection=stepwise;;
run;
proc glmselect data=HIVpmc;
title" select VCAS for GDS using transformed data";
class GFRc coc;
model gds1=age cd4_rec vir_recv hepc_mr HGB PLT ALT AST GFRc  COC;
run;
