MODULE testcomparisons ;


FROM libc IMPORT printf, exit ;
FROM Builtins IMPORT isgreater, isless, islessequal, isgreaterequal ;
FROM SYSTEM IMPORT ADR ;


(*
   assert -
*)

PROCEDURE assert (value: BOOLEAN; message: ARRAY OF CHAR) ;
BEGIN
   IF NOT value
   THEN
      printf ("test failed: %s\n", ADR (message)) ;
      code := 1
   END
END assert ;


(*
   test -
*)

PROCEDURE test ;
VAR
   result: INTEGER ;
BEGIN
   result := isgreater (2.0, 1.0) ;
   printf ("isgreater (2.0, 1.0) = %d\n", result) ;
   assert (result = 1, "isgreater (2.0, 1.0) # 1") ;

   result := isless (1.0, 2.0) ;
   printf ("isless (1.0, 2.0) = %d\n", result) ;
   assert (result = 1, "isless (1.0, 2.0) # 1") ;

   result := islessequal (1.0, 2.0) ;
   printf ("islessequal (1.0, 2.0) = %d\n", result) ;
   assert (result = 1, "islessequal (1.0, 2.0) # 1") ;

   result := isgreaterequal (2.0, 1.0) ;
   printf ("isgreaterequal (2.0, 1.0) = %d\n", result) ;
   assert (result = 1, "isgreatereequal (2.0, 1.0) # 1") ;

   result := isgreater (1.0, 2.0) ;
   printf ("isgreater (1.0, 2.0) = %d\n", result) ;
   assert (result = 0, "isgreater (1.0, 2.0) # 0") ;

   result := isless (2.0, 1.0) ;
   printf ("isless (2.0, 1.0) = %d\n", result) ;
   assert (result = 0, "isless (2.0, 1.0) # 0") ;

   result := islessequal (2.0, 1.0) ;
   printf ("islessequal (2.0, 1.0) = %d\n", result) ;
   assert (result = 0, "islessequal (2.0, 1.0) # 0") ;

   result := isgreaterequal (1.0, 2.0) ;
   printf ("isgreaterequal (1.0, 2.0) = %d\n", result) ;
   assert (result = 0, "isgreatereequal (1.0, 2.0) # 1")
END test ;


VAR
   code: INTEGER ;
BEGIN
   code := 0 ;
   test ;
   IF code = 0
   THEN
      printf ("all tests pass\n")
   ELSE
      printf ("some tests failed\n")
   END ;
   exit (code)
END testcomparisons.
