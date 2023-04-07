MODULE hello ;

FROM liba IMPORT bar ;
FROM libc IMPORT printf ;

BEGIN
   bar ;
   printf ("hello world\n")
END hello.
