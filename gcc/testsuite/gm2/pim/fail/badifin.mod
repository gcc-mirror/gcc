MODULE badifin ;

TYPE
   enums = (red, blue, green) ;
   set = SET OF enums ;
VAR
   setvar : set;
   enumvar: enums;
BEGIN
   setvar := set {red, blue} ;
   IF NOT (setvar IN setvar)
   THEN
      HALT
   END
END badifin.
