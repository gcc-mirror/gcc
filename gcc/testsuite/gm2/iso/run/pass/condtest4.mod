MODULE condtest4 ;

FROM libc IMPORT printf, exit ;


PROCEDURE test (VAR a, b: BOOLEAN) ;
BEGIN
   IF a AND b
   THEN
      printf ("passed\n")
   ELSE
      printf ("failed\n") ;
      exit (1)
   END
END test ;


VAR
   e, f: BOOLEAN ;
BEGIN
   e := TRUE ;
   f := TRUE ;
   test (e, f)
END condtest4.
