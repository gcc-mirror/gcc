MODULE condtest3 ;

FROM libc IMPORT printf, exit ;


PROCEDURE test ;
CONST
   a = 1 ;
   b = 2 ;
   c = 3 ;
   d = 3 ;
   Result = ((a = b) # (c = d)) ;
BEGIN
   IF Result
   THEN
      printf ("passed\n")
   ELSE
      printf ("failed\n") ;
      exit (1)
   END
END test ;


BEGIN
   test
END condtest3.
