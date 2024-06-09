MODULE testinittm ;

FROM wraptime IMPORT InitTM, tm ;
FROM libc IMPORT printf, exit ;

VAR
   m: tm ;
BEGIN
   m := InitTM () ;
   IF m = NIL
   THEN
      printf ("InitTM failed\n");
      exit (1)
   ELSE
      printf ("InitTM passed\n")
   END
END testinittm.
