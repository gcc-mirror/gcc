IMPLEMENTATION MODULE b ;

IMPORT a ;
FROM libc IMPORT printf ;


BEGIN
   printf ("init: module b\n")
FINALLY
   printf ("finish: module b\n")
END b.
