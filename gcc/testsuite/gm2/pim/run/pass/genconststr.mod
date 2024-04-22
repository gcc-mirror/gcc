MODULE genconststr ;

FROM libc IMPORT printf, exit ;
FROM StrLib IMPORT StrEqual ;

CONST
   foo = hello + space + world ;
   hello = "hello" ;
   space = " " ;
   world = "world" ;

PROCEDURE test (a: ARRAY OF CHAR) ;
BEGIN
   IF NOT StrEqual (a, "hello world")
   THEN
      printf ("const string failed\n");
      exit (1)
   END
END test ;

BEGIN
   test (foo)
END genconststr.
