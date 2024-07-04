MODULE hashfirstcolumn2 ;

FROM libc IMPORT printf, exit ;

VAR
   x, y: CARDINAL ;
BEGIN
   x := 1 ;
   y := 2 ;
   IF x
# y
   THEN
      printf ("success\n");
   ELSE
      printf ("failure\n");
      exit (1)
   END
END hashfirstcolumn2.
