MODULE longint3 ;

FROM libc IMPORT exit, printf ;
FROM Builtins IMPORT log10l ;

PROCEDURE test ;
VAR
     li  : LONGINT ;
     in,
     res : LONGREAL ;
BEGIN
     li := 123 ;
     in := VAL (LONGREAL, li) ;
     res := log10l (in) ;
     printf ("log10l (%Lf) = %Lf\n", in, res);
     in := VAL (REAL, li) ;
     res := log10l (in) ;
     printf ("log10l (%Lf) = %Lf\n", in, res);
END test ;

BEGIN
   test
END longint3.
