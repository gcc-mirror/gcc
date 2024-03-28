MODULE localmodule ;

FROM libc IMPORT printf ;

PROCEDURE mult2 (n: CARDINAL) : CARDINAL ;
BEGIN
   RETURN 2*n
END mult2 ;

MODULE local ;

  IMPORT mult2 ;
  EXPORT mysqr ;

  PROCEDURE mysqr (n: CARDINAL) : CARDINAL ;
  BEGIN
     RETURN mult2 (n) * mult2 (n)
  END mysqr ;

END local ;

VAR
   d: CARDINAL ;
BEGIN
   d := mysqr (3) ;
   printf ("sqr (3 * 2) = %d\n", d)
END localmodule.
