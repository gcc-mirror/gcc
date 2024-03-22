MODULE badassignment ;

TYPE
   enums = (red, blue, green) ;
   set = SET OF enums ;
VAR
   setvar : set;
   enumvar: enums;
BEGIN
   enumvar := 'a';
   enumvar := 'ab';
   setvar := 'a';
   setvar := 'ab';
END badassignment.