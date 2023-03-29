MODULE nestedset ;

TYPE
   someset = SET OF [0..15] ;

PROCEDURE a (s : someset) ;
BEGIN
END a ;

PROCEDURE b (s : someset) ;

   PROCEDURE c ;
   BEGIN
      a(s);
   END c ;

BEGIN
END b;

BEGIN
END nestedset.

