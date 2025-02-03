MODULE printtypesize ;  

FROM libc IMPORT printf ;
FROM SYSTEM IMPORT SIZE, CSIZE_T, COFF_T ;

BEGIN
   printf ("SIZE (CHAR) = %d bytes\n", SIZE (CHAR));
   printf ("SIZE (INTEGER) = %d bytes\n", SIZE (INTEGER));
   printf ("SIZE (CARDINAL) = %d bytes\n", SIZE (CARDINAL));
   printf ("SIZE (BITSET) = %d bytes\n", SIZE (BITSET));
   printf ("SIZE (LONGINT) = %d bytes\n", SIZE (LONGINT));
   printf ("SIZE (LONGCARD) = %d bytes\n", SIZE (LONGCARD));
   printf ("SIZE (CSIZE_T) = %d bytes\n", SIZE (CSIZE_T));
   printf ("SIZE (COFF_T) = %d bytes\n", SIZE (COFF_T));   
END printtypesize.
