MODULE badbecomes ;

TYPE
   enums = (red, blue, green) ;
   set = SET OF enums ;
VAR
   setvar : set;
BEGIN
   setvar := green ;    (* Should detect an error here.  *)
END badbecomes.
