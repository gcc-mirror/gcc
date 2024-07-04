MODULE badexpression2 ;

TYPE
   enums = (red, blue, green) ;
   set = SET OF enums ;
VAR
   setvar : set;
   enumvar: enums;
BEGIN
   enumvar := 'ab';
   setvar := set {} ;
   setvar := setvar + "hello"
END badexpression2.
