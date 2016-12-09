/*====================================================================
Acknowledgement: The following codes were copied and modified from the 
scripts published by "Ocean Bancova" (one of privious Bancova students) 
to generate the currently assigned demog table".
=====================================================================*/ 

dm "log;clear;out;clear";
/*====================================================================
| PROJECT:          BANCOVA SAS TRAINING
|                   INTRODUCTION TO BANCOVA SAS
|
| PROGRAM:          DEMOG.SAS
| PROGRAMMER(S):    Yonghua Zhuang
| DATE:             06/22/2016
| PURPOSE:          SUBJECT DEMOGRAPHICS 
|                        
|
| QC PROGRAMMER(S):
| QC DATE:
|
| INPUT FILE DIRECTORY(S):          
| OUTPUT FILE DIRECTORY(S):         
| OUTPUT AND PRINT SPECIFICATIONS: T01_DEMOG.RTF    
|
|
| REVISION HISTORY
| DATE     BY        COMMENTS
|
|
=====================================================================
|COMMENTS:
| a) The statistical output found in the *.LIS is difficult to interpret as the treatment and order variables are in their numeric form.  
|    Format and label the variables used in PROC FREQ and PROC GLM analyses.  Title the output appropriately.  
|    Use the #byval (order) statement and the orderf format to make the title, and the trtf format to format the treatment. 
| b) Usually, the statistical analyses are printed and placed in the appendix of a report as supporting documentation.  
|    Thus to separate the QC and miscellaneous data listings from the statistical output, route the statistical output to a *.
|    STT file using the PRINTTO procedure.  The file should be named demog4.stt. 
=====================================================================*/ 
 
options ps=77 ls=100 center formchar="|____|||___+=|_/\<>*" missing='.' nobyline nodate  
pagno=1 ; run;

* %let raw = C:\bancova2016summer\data\raw;
* %let derived = C:\bancova2016summer\data\derived; 
* run;

title 'BANCOVA SAS Training:  Patient Demographics' ; 
run;

Proc format;
	value sectno
		1 = 'Gender N(%)'
		2 = 'Race N(%)'
		3 = 'Age (/years)'
		4 = 'Height (cm)'
		5 = 'Weight (kg)'
		6 = 'BMI (kg/m^{super 2})';
	value $ gender
		'MALE' = 'Male'
		'FEMALE' = 'Female';
	value $ race
		'CAUCASIA' = 'Caucasia'
		'BLACK' = 'Black'
		'ASIAIAN' = 'Asiaian';
	value $ stat
		'MEAN' = 'Mean'
		'MEDIAN' = 'Median'
		'STD' = 'Std.Dev.'
		'MIN' = 'Minimum'
		'MAX' = 'Maximum';
run;


Data demog;
	infile "C:\bancova2016summer\data\raw\Demog_data.csv" delimiter = ',' missover dsd firstobs = 2;
	input visit gender$ race$ birthdtf:yymmdd8. subjid treatment height hgtunit weight wtgunit itt safety;
	today = date();
	age = floor((today - birthdtf)/365);
	bmi = (weight / (height/100 * height/100) );
drop visit today hgtunit wtgunit; 
if itt;
run;


title "Frequence table of Categorical variables (gender,race)";
Proc freq data = work.demog;
	tables gender race;
run; 

title "Summary of continuous variables (height, weight, age, bmi)";
Proc means data = work.demog;
 var height age weight ;
run;

Proc sort data = demog;
	by subjid;
run; 

data demog dupes;
	set demog;
	by subjid;
	if first.subjid and last.subjid then output demog;
	else output dupes;
run;


PROC CONTENTS data = demog; run;
PROC CONTENTS data = dupes; run;
          

Proc sort data = work.demog;
	by treatment descending gender descending race;
run;

Proc freq data = demog order = data;
	by treatment;
	tables gender / out = gender0 noprint;
	table race /out = race0 noprint;
run;

Proc freq data = demog order = data;
	tables gender / out = gender00 noprint;
	table race /out = race00 noprint;
run;

Data gender00;
	set gender00;
	treatment = .;
run;

Data race00;
	set race00;
	treatment = .;
run;

data gender0;
   merge gender0 gender00;
   by treatment;
run;

data race0;
   merge race0 race00;
   by treatment;
run;


%macro stdstat(input, variable, output);
	Proc means data=&input;
		class treatment;
		var &variable;
		output out =&output;
		output out = median median=;
	run;
	Data &output;
   		set &output median(in=in2);
   		by _type_ treatment;
   		if in2 then _STAT_ = 'MEDIAN';
   run;
%mend;


%macro pvalue(input, variable, output);
	Proc freq data=&input noprint;
		tables treatment*&variable /chisq expected norow nocol nopercent;
		output out =&output chisq;
	run;
%mend;

%macro pvalue_con(input, variable, output);
	Proc npar1way data=&input wilcoxon noprint;
		class treatment;
		var &variable;
		output out =&output wilcoxon;
	run;
%mend;


%stdstat(demog, age, age0);
%stdstat(demog, height, height0);
%stdstat(demog, weight, weight0);
%stdstat(demog, bmi, bmi0);


%pvalue(demog, gender, gender1);
%pvalue(demog, race, race1);

%pvalue_con(demog, age, age1);
%pvalue_con(demog, height, height1);
%pvalue_con(demog, weight, weight1);
%pvalue_con(demog, bmi, bmi1);


data all0;
	set gender0 (in = a)
		race0 	(in = b)
		age0	(in = c)
		height0 (in = d)
		weight0 (in = e)
		bmi0 	(in = f);
	keep sectno treatment statcode statname statrslt;
	sectno = (a * 1)+ (b * 2) + (c * 3) +(d * 4) +(e * 5) +(f * 6);
	if a then do;
		statcode = gender;
		statname = put (gender, gender.);
		statrslt = put (count, 3.) ||'('||put(trim(percent), 4.)||')';
	end;

	if b then do;
		statcode = race;
		statname = put (race, race.);
		statrslt = put (count, 3.) ||'('||put(trim(percent), 4.)||')';
	end;

	if c then do;
		statcode = _STAT_;
		statname = put (_STAT_, stat.);
		statrslt = put (age, 3.);
	end;

	if d then do;
		statcode = _STAT_;
		statname = put (_STAT_, stat.);
		statrslt = put (height, 3.);
	end;

	if e then do;
		statcode = _STAT_;
		statname = put (_STAT_, stat.);
		statrslt = put (weight, 3.);
	end;

	if f then do;
		statcode = _STAT_;
		statname = put (_STAT_, stat.);
		statrslt = put (bmi, 3.);
	end;

run;

Proc print data= all0; run;


data pvalue;
	set gender1 (in = a)
		race1 	(in = b)
		age1	(in = c)
		height1 (in = d)
		weight1 (in = e)
		bmi1 	(in = f);
	keep sectno v3;
	sectno = (a * 1)+ (b * 2) + (c * 3) +(d * 4) +(e * 5) + (f * 6);
	if a then do;
		v3 = P_AJCHI;
	end;

	if b then do;
		v3 = P_PCHI;
	end;

	if c then do;
		v3 = P2_WIL;
	end;

	if d then do;
		v3 = P2_WIL;
	end;

	if e then do;
		v3 = P2_WIL;
	end;

	if f then do;
		v3 = P2_WIL;
	end;
		
run;

data all0;
	set all0;
	length = length (statcode);
run;

Proc sort data = all0;
	by sectno length statcode  statname;
run;


Proc transpose data = all0 out = all1 prefix =Variable;
	by sectno length statname;
	var statrslt;
run;

Data all1;
	set all1;
	drop length;
run;


data all2;
	merge all1 pvalue;
	by sectno;
	if not first.sectno then v3='.';
run;

proc print data = all2; run;


*------------------------------------------------------------*;
*Generate report
*------------------------------------------------------------*;
PROC TEMPLATE;
 DEFINE STYLE all_style;
	class systemtitle / fontfamily = "Arial" fontsize = 3 color = black;
	class systemfooter / just = left fontfamily = "Arial" fontsize = 3 color = black;
	class header / just = left fontfamily = "Arial" fontsize = 2 fontstyle = roman fontweight = medium color = black backgroundcolor = white;
	class table / borderwidth = 2 cellpadding = 2.5 frame = hsides bordertopstyle = solid borderbottomstyle = solid rules = groups;
	class data /fontfamily = "Arial" fontsize = 2 color = black backgroundcolor = white;
	class body / bottommargin = 1 in topmargin = 1 in rightmargin =  1 in leftmargin = 1 in;
END; 
RUN;


ods rtf style = all_style file = "C:\bancova2016summer\data\derived\demog.rtf";
ods escapechar='^';
title 'Patient Demographic' justify = center;
title2 '(ITT POPULATION)*';
footnote '*ITT (Intent to Treat) population is defined as including all subjects who have at least one baseline assessment and one post baseline assessment.';
footnote2 '**N is the number of subjects in the treatment group.';
footnote3 '*** P values were obtained using two sided Fisher exact test for Gender and Race,';
footnote4 'and using rank test for Age, Height, and weight.';

Proc report data = all2 nowindows headline  split ='|';
	Format variable $20.;
	column sectno statname ((variable1 variable2 variable3 v3));
	define sectno /group noprint;
	define statname /group 'variable'  style(column)=[cellwidth=1.5in] order order = data;
	define statname /group '' style(column)=[cellwidth=1.5in] order order = data;
	define variable1 /display 'Placebo' style(column)=[cellwidth=1in] ;
	define variable1 /display '(N=9)**'  style(column)=[cellwidth=1in]; *did not figure out how to cite N yet;
	define variable2 /display 'Drug'  style(column)=[cellwidth=1in];
	define variable2 /display '(N=9)**'  style(column)=[cellwidth=1in]; *did not figure out how to cite N yet;
	define variable3 /display 'Total'  style(column)=[cellwidth=1in];
	define variable3 /display '(N=18)**' style(column)=[cellwidth=1in]; *did not figure out how to cite N yet;
	define v3 /display 'p Value(***)' Format = 6.3 style(column)=[cellwidth=1in];
		compute before sectno /style={just=l};
			line ' ';
			line  sectno sectno. ;
			endcomp;
		quit;
run;

ods rtf close;

*------------------------------------------------------------*;
*Generate listing
*------------------------------------------------------------*;
Proc sort data = demog;
	by treatment subjid;
run;

title 'DEMOGRAPHICS AND BASELINE CHARACTERISTICS ';
title2 '(ITT POPULATION)*';
footnote '*ITT (Intent to Treat) population is defined as including all subjects who have at least one baseline assessment and one post baseline assessment.';
footnote2 '**N is the number of subjects in the treatment group.';

Proc print data = demog;
	var treatment gender race age height weight;
	id subjid;
	label age = 'Age (years)' height = 'Height (cm)' weight = 'Weight (kg)';
run;

*------------------------------------------------------------*;
*Generate figure
*------------------------------------------------------------*;
title 'Histogram of Ages for All treatment Groups' justify = center;
title2 '(ITT POPULATION)' justify = center ;

Proc univariate data = demog;
	var age ;
	histogram;
run;































