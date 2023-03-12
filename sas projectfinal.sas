/*數值類敘述統計及相關係數*/
PROC CORR DATA=POR;
VAR age Medu Fedu traveltime studytime failures famrel freetime goout Dalc Walc health absences G1 G2 G3 grades;
RUN;
/*one hot encoding*/
data por1;
set por;
grades=(G1+G2+G3)/3;
if grades>=12 then HL=1;
if grades<12 then HL=0;
if school='GP'  then school1=1;
if school='MS' then school1=0;
if sex='M' then sex1=1;
if sex= 'F' then sex1=0;
if address='U' then address1=1;
if address='R' then address1=0;
if famsize='GT3' then famsize1=1;
if famsize='LE3' then famsize1=0;
if Pstatus='T' then Pstatus1=1;
if Pstatus='A' then  Pstatus1=0;
if schoolsup='yes' then schoolsup1=1;
if schoolsup='no' then schoolsup1=0;
if famsup='yes' then famsup1=1;
if famsup='no' then famsup1=0;
if paid='yes' then paid1=1;
if paid='no' then paid1=0;
if activities='yes' then activities1=1;
if activities='no' then activities1=0;
if nursery='yes' then nursery1=1;
if nursery='no' then nursery1=0;
if higher='yes' then higher1=1;
if higher='no' then higher1=0;
if internet='yes' then internet1=1;
if internet='no' then internet1=0;
if romantic='yes' then romantic1=1;
if romantic='no' then romantic1=0;
if Mjob='health' then Mjobhealth=1;  else Mjobhealth=0;
if Mjob='teacher' then Mjobteacher=1;  else Mjobteacher=0;
if Mjob='services' then Mjobservices=1;  else Mjobservices=0;
if Mjob='other' then Mjobother=1;  else Mjobother=0;
if Fjob='health' then Fjobhealth=1;  else Fjobhealth=0;
if Fjob='teacher' then Fjobteacher=1;  else Fjobteacher=0;
if Fjob='services' then Fjobservices=1;  else Fjobservices=0;
if Fjob='other' then Fjobother=1;  else Fjobother=0;
if reason='reputation' then reasonreputation=1;  else reasonreputation=0;
if reason='course' then reasoncourse=1;  else reasoncourse=0;
if reason='other' then reasonother=1;  else reasonother=0;
if guardian='father' then guardianfather=1;  else guardianfather=0;
if guardian='mother' then guardianmother=1;  else guardianmother=0;
drop school sex address famsize Pstatus schoolsup famsup paid activities nursery higher internet romantic G1 G2 G3 Mjob Fjob reason guardian;
RUN;
/*檢驗內生性問題*/
PROC REG;
model grades=age Medu Fedu traveltime studytime failures famrel freetime goout Dalc Walc health absences school1 sex1 address1 famsize1 pstatus1 schoolsup1 famsup1 paid1 activities1 nursery1 higher1 internet1 romantic1 Mjobhealth Mjobteacher Mjobservices Mjobother Fjobhealth Fjobteacher Fjobservices Fjobother reasonreputation reasoncourse reasonother guardianmother guardianfather / vif collin STB DW R INFLUENCE;
output out=inf h=hhat cookd=cook covratio=cov dffits=dffits rstudent=restudent;
RUN;
QUIT;
/*逐步選取法挑變數跑回歸*/
proc reg data=por1;
model grades=Medu Fedu traveltime studytime failures famrel freetime goout Dalc Walc health absences school1 sex1 address1 famsize1 pstatus1 schoolsup1 famsup1 paid1 activities1 nursery1 higher1 internet1 romantic1 Mjobhealth Mjobteacher Mjobservices Mjobother Fjobhealth Fjobteacher Fjobservices Fjobother reasonreputation reasoncourse reasonother guardianmother guardianfather /CLB selection=stepwise;
run;
quit;
data por2;
set por;
grades=(G1+G2+G3)/3;
if grades>=12 then HL=1;
if grades<12 then HL=0;
if failures>0 then failures1="drop";
if failures=0 then failures1="pass";
RUN;
proc hpsplit data=por2 maxdepth=10; /*the maximum level of a tree is 6*/
   class  HL school sex address famsize Pstatus Mjob Fjob reason guardian  schoolsup famsup paid activities nursery higher internet romantic;
   model HL(events='1')=  /*The targe variable is "Bad", and the event is defind as Bad=1*/
             school sex age address famsize Pstatus Mjob Fjob reason guardian Medu Fedu traveltime studytime failures famrel freetime goout Dalc Walc health absences schoolsup famsup paid activities nursery higher internet romantic; /*These are variables used for classification*/
  partition fraction(validate=0.3 seed=123);
run;
/*作圖前排序*/
proc sort data=por1;
	by school;
proc boxplot data=por1;
	plot grades*school;
	run;
quit;
proc anova data=por2; 
  class failures1 higher ; 
  model grades=failures1 higher ; 
  means failures1 higher  / TUKEY CLDIFF;
run;
quit;
PROC UNIVARIATE DATA=por;
  VAR  age Medu Fedu traveltime studytime failures famrel freetime goout Dalc Walc health absences;
  HISTOGRAM age Medu Fedu traveltime studytime failures famrel freetime goout Dalc Walc health absences /normal;
  run;
  PROC UNIVARIATE DATA=por2 normal;
  VAR  grades;
  histogram grades ;
  run;
  proc univariate data=por;
  class failures;
  var failures;     
  histogram failures ;
  ods select histogram;
run;
ods graphics on;
ods html;
proc sgplot data=por6;/*資料檔名為mix*/
	title "drop&no"; 
    reg  x=Dalc y=grades/ legendlabel="dalc";/*告知x軸與y軸的變項*/
	reg  x=Walc y=grades/ legendlabel="walc";
run;
ods html close;
ods graphics off;
ods _all_ close;






















