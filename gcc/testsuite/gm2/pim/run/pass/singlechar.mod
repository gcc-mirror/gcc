MODULE singlechar ;

FROM libc IMPORT printf, exit ;
FROM StrLib IMPORT StrLen ;


PROCEDURE input (a: ARRAY OF CHAR) ;
BEGIN
   IF StrLen (a) # 1
   THEN
      printf ("string length is not 1, but %d\n", StrLen (a)) ;
      exit (1)
   END
END input ;


BEGIN
   input (015C) ;
   printf ("successful test, finishing\n")
END singlechar.
