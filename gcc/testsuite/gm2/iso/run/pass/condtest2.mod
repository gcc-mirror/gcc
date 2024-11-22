MODULE condtest2 ;

FROM libc IMPORT printf, exit ;


PROCEDURE test (VAR a, b, c, d: CARDINAL) ;
BEGIN
   IF (a = b) # (c = d)
   THEN
      printf ("passed\n")
   ELSE
      printf ("failed\n") ;
      exit (1)
   END
END test ;


VAR
   e, f, g, h: CARDINAL ;
BEGIN
   e := 1 ;
   f := 2 ;
   g := 3 ;
   h := 3 ;
   test (e, f, g, h)
END condtest2.
