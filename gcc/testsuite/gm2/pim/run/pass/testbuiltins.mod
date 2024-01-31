MODULE testbuiltins ;  (*!m2pim+gm2*)

FROM Builtins IMPORT log10l ;
FROM libc IMPORT printf, exit ;
FROM libm IMPORT powl ;


(*
   doPowerOfTen - safely returns the exponent of a LONGREAL as an INTEGER.
*)

PROCEDURE doPowerOfTen (r: LONGREAL) : INTEGER ;
VAR
   i   : INTEGER ;
   c, d: LONGREAL ;
BEGIN
   IF r=0.0
   THEN
      RETURN( 0 )
   ELSE
      IF r<0.0
      THEN
         c := -r
      ELSE
         c := r
      END ;
      IF c>=1.0
      THEN
         RETURN( VAL(INTEGER, log10l (c)) )
      ELSE
         i := 0 ;
         LOOP
            d := c*powl(10.0, VAL(LONGREAL, i)) ;
            IF d>=1.0
            THEN
               RETURN( -i )
            ELSE
               INC(i)
            END
         END
      END
   END
END doPowerOfTen ;


PROCEDURE test ;
BEGIN
   Assert (doPowerOfTen (1.0), 0) ;
   Assert (doPowerOfTen (10.0), 1) ;
   Assert (doPowerOfTen (100.0), 2) ;
   Assert (doPowerOfTen (-1.0), 0) ;
   Assert (doPowerOfTen (-10.0), 1) ;
   Assert (doPowerOfTen (-100.0), 2)
END test ;


(*
   Assert -
*)

PROCEDURE Assert (func, actual: INTEGER) ;
BEGIN
   IF func = actual
   THEN
      printf ("success:  computed %d = actual %d\n", func, actual)
   ELSE
      printf ("failure:  computed %d # actual %d\n", func, actual) ;
      code := 1
   END
END Assert ;


VAR
   code: INTEGER ;
BEGIN
   code := 0 ;
   test ;
   exit (code)
END testbuiltins.
