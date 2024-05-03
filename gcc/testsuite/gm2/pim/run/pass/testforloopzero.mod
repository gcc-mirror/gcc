MODULE testforloopzero ;

FROM libc IMPORT printf, exit ;


(*
   test -
*)

PROCEDURE test ;
VAR
   i, n,
   count: CARDINAL ;
BEGIN
   n := 5 ;
   count := 0 ;
   FOR i := n TO 0 BY -1 DO
      printf ("i = %d, count = %d\n", i, count);
      INC (count)
   END ;
   IF count = 6
   THEN
      printf ("for loop counting down passed\n")
   ELSE
      printf ("for loop counting down failed\n") ;
      exit (1)
   END
END test ;


BEGIN
   test
END testforloopzero.
