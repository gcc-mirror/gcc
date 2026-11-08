IMPLEMENTATION MODULE a ;

FROM libc IMPORT printf ;

BEGIN
   printf ("init: module a\n")
FINALLY
   printf ("finish: module a\n")
END a.
