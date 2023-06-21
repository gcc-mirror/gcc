MODULE addcharconst ;

FROM libc IMPORT printf, exit ;
FROM StrLib IMPORT StrLen ;


PROCEDURE input (a: ARRAY OF CHAR) ;
BEGIN
   IF StrLen (a) # 2
   THEN
      printf ("string length is not 2, but %d\n", StrLen (a)) ;
      exit (1)
   END
END input ;


BEGIN
   input (015C + 012C) ;
   printf ("successful test, finishing\n")
END addcharconst.
