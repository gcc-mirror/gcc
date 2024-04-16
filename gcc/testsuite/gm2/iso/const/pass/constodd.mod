MODULE constodd ;

FROM libc IMPORT printf, exit ;

CONST
   IsOdd = ODD (1) AND (2 > 1) ;

BEGIN
   IF IsOdd
   THEN
      printf ("success\n");
   ELSE
      printf ("failure\n");
      exit (1)
   END
END constodd.
