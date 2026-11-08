IMPLEMENTATION MODULE c ;

IMPORT b ;
FROM libc IMPORT printf ;

BEGIN
   printf ("init: module c\n")
FINALLY
   printf ("finish: module c\n")
END c.
