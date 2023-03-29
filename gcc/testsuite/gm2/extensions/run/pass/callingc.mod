MODULE callingc ;

FROM libc IMPORT printf, exit ;
FROM StrLib IMPORT StrLen ;

VAR
   a: ARRAY [0..1] OF CHAR ;
BEGIN
   printf ("\n") ;
   IF StrLen ("\n") # 2
   THEN
      printf ("yes the conversion of the string into printf has corrupted the literal version as well!\n") ;
      exit (1)
   END ;
   a := "\n" ;
   IF StrLen (a) # 2
   THEN
      printf ("yes the conversion of the string into printf has corrupted the array version as well!\n") ;
      exit (2)
   END
END callingc.
