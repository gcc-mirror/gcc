MODULE badbecomes2 ;

TYPE
   enums = (red, blue, green) ;
VAR
   setvar: SET OF enums ;
BEGIN
   setvar := green ;    (* Should detect an error here.  *)
END badbecomes2.
