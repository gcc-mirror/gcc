MODULE badexpression2 ;

TYPE
   enums = (red, blue, green) ;
   set = SET OF enums ;
VAR
   setvar : set;
   enumvar: enums;
BEGIN
   setvar := set {red, blue} ;
   enumvar := green ;
   setvar := setvar + enumvar ;    (* Should detect an error here.  *)
   IF NOT (green IN setvar)
   THEN
      HALT
   END
END badexpression2.
