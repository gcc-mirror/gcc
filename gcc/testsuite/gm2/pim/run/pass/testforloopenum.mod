MODULE testforloopenum ;

FROM libc IMPORT printf, exit ;

TYPE
   colour = (red, green, blue, yellow) ;

PROCEDURE test ;
VAR
   c    : colour ;
   count: CARDINAL ;
BEGIN
   count := 0 ;
   FOR c := red TO blue BY colour (2) DO
      INC (count) ;
      printf ("c = %d, count = %d\n", c, count)
   END ;
   IF count = 2
   THEN
      printf ("passed\n")
   ELSE
      printf ("failed\n") ;
      exit (1)
   END
END test ;


BEGIN
   test
END testforloopenum.
