MODULE testforloopchar ;

FROM libc IMPORT printf, exit ;

PROCEDURE test ;
VAR
   ch   : CHAR ;
   count: CARDINAL ;
BEGIN
   count := 0 ;
   FOR ch := 'a' TO 'z' DO
      INC (count) ;
      printf ("ch = %c, count = %d\n", ch, count)
   END ;
   IF count = 26
   THEN
      printf ("passed\n")
   ELSE
      printf ("failed\n") ;
      exit (1)
   END
END test ;


BEGIN
   test
END testforloopchar.
