MODULE testforloopzero2 ;

FROM libc IMPORT printf, exit ;


(*
   test -
*)

PROCEDURE test ;
VAR
   i, n,
   zero,
   count: CARDINAL ;
BEGIN
   n := 5 ;
   count := 0 ;
   zero := 0 ;
   FOR i := n TO zero BY -1 DO
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
END testforloopzero2.
