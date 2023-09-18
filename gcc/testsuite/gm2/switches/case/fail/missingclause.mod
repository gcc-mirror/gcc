MODULE missingclause ;  (*!m2iso+gm2*)


TYPE
   colour = (red, green, blue) ;


PROCEDURE init (c: colour) ;
BEGIN
   CASE c OF

   red,
   blue: (* User forgets green.  *)

   END
END init ;


VAR
   rgb: colour ;
BEGIN
   init (rgb)
END missingclause.
