MODULE convert1 ;

FROM libc IMPORT printf, exit ;
FROM Builtins IMPORT log10l ;


PROCEDURE assert (a, b: INTEGER) ;
BEGIN
   IF a # b
   THEN
      printf ("failure: %d # %d\n", a, b) ;
      code := 1
   END
END assert ;


PROCEDURE identityl (l: LONGREAL) : LONGREAL ;
BEGIN
   RETURN l
END identityl ;


PROCEDURE foo (l: LONGREAL) : INTEGER ;
BEGIN
   RETURN VAL (INTEGER, identityl (l))
END foo ;


PROCEDURE fooi (l: LONGREAL) : INTEGER ;
BEGIN
   RETURN VAL (INTEGER, log10l (l))
END fooi ;


PROCEDURE test ;
BEGIN
   assert (foo (1.0), 1) ;
   assert (fooi (100.0), 2) ;
   assert (fooi (1.23456789), 0)
END test ;


VAR
   code: INTEGER ;
BEGIN
   code := 0 ;
   test ;
   exit (code)
END convert1.
