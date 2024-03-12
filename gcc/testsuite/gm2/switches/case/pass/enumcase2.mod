MODULE enumcase2 ;  (*!m2iso+gm2*)


TYPE
   colour = (red, blue, green) ;

PROCEDURE init (c: colour) ;
BEGIN
   CASE c OF

   red:  |
   blue..green:

   END
END init ;


VAR
   rgb: colour ;
BEGIN
   init (rgb)
END enumcase2.
