MODULE longint2 ;

FROM libc IMPORT printf, exit ;
FROM Builtins IMPORT log10l, fabsl ;


PROCEDURE equals (left, right: LONGREAL) : BOOLEAN ;
CONST
   epsilon = 0.001 ;
VAR
   diff: LONGREAL ;
BEGIN
   IF right > left
   THEN
      diff := right - left
   ELSE
      diff := left - right
   END ;
   RETURN diff <= epsilon
END equals ;


PROCEDURE test (function: ARRAY OF CHAR; input, output, result: LONGREAL) ;
BEGIN
   printf ("function %s (%Lf) = %Lf", function, input, output) ;
   IF equals (output, result)
   THEN
      printf (" passed")
   ELSE
      printf (" (incorrect and should be: %Lf)", result) ;
      code := 1
   END ;
   printf ("\n")
END test ;


VAR
   code: INTEGER ;
   li  : LONGINT ;
   in,
   res : LONGREAL ;
BEGIN
   code := 0 ;
   li := 123 ;
   in := VAL (LONGREAL, li) ;
   res := log10l (in) ;
   test ("log10l", in, res, 2.0899) ;
   res := fabsl (in) ;
   test ("fabsl", in, res, 123.0) ;
   exit (code)
END longint2.
