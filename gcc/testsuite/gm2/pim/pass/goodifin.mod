MODULE goodifin ;

TYPE
   enums = (red, blue, green) ;
   set = SET OF enums ;
VAR
   setvar : set;
   enumvar: enums;
BEGIN
   setvar := set {red, blue} ;
   IF green IN setvar
   THEN
      HALT
   END
END goodifin.
