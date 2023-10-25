MODULE testnan ;

FROM Builtins IMPORT isnan ;
FROM libc IMPORT printf, exit ;

VAR
   x: REAL ;
BEGIN
   x := 0.0 / 0.0 ;
   IF isnan (x) = 1
   THEN
      printf ("success isnan working from module Builtins\n")
   ELSE
      printf ("failure isnan is not working from module Builtins\n") ;
      exit (1)
   END
END testnan.
