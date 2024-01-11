MODULE badexpression3 ;

TYPE
   enums = (red, blue, green) ;
   set = SET OF enums ;
VAR
   setvar : set;
   enumvar: enums;
BEGIN
   setvar := set {red, blue} ;
   setvar := setvar + green ;    (* Should detect an error here.  *)
   IF NOT (green IN setvar)
   THEN
      HALT
   END
END badexpression3.