MODULE testforloopzero4 ;

FROM libc IMPORT printf, exit ;


(*
   test -
*)

PROCEDURE test ;
VAR
   i,
   count: INTEGER ;
BEGIN
   count := 0 ;
   FOR i := 5 TO -5 BY -1 DO
      printf ("i = %d, count = %d\n", i, count);
      INC (count)
   END ;
   IF count = 11
   THEN
      printf ("for loop counting down (%d) passed\n", count)
   ELSE
      printf ("for loop counting down (%d) failed\n", count) ;
      exit (1)
   END
END test ;


BEGIN
   test
END testforloopzero4.
