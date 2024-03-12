MODULE test ;  (*!m2pim+gm2*)

IMPORT cstub ;
FROM libc IMPORT printf, exit ;
FROM Builtins IMPORT sinl, sin, cosl, cos, log10, log10l ;


CONST
   epsilon = 0.001 ;
   pi = 3.1415927 ;

VAR
   code: INTEGER ;


PROCEDURE near (x, y: REAL) ;
VAR
   diff: REAL ;
BEGIN
   diff := x - y ;
   IF diff < 0.0
   THEN
      diff := -diff
   END ;
   IF diff > epsilon
   THEN
      printf ("failure: %f, %f differ\n", x, y) ;
      code := 1
   END
END near ;


PROCEDURE nearl (in, out, correct: LONGREAL; func: ARRAY OF CHAR) ;
VAR
   diff: LONGREAL ;
BEGIN
   diff := out - correct ;
   IF diff < 0.0
   THEN
      diff := -diff
   END ;
   IF diff > epsilon
   THEN
      printf ("failure: %s ( %Lf ) -> %Lf # %Lf \n", func, in, out, correct) ;
      code := 1
   END
END nearl ;


PROCEDURE testfunc ;
VAR
   l: LONGREAL ;
   r: REAL ;
BEGIN
   near (1.0, 1.0) ;
   near (10.0, 10.0) ;
   near (100.0, 100.0) ;
   nearl (1.0, 1.0, 1.0, "") ;
   nearl (10.0, 10.0, 10.0, "") ;
   nearl (100.0, 100.0, 100.0, "") ;

   near (cstub.identical (1.0), 1.0) ;
   near (cstub.identical (10.0), 10.0) ;
   near (cstub.identical (100.0), 100.0) ;
   nearl (cstub.identicall (1.0), 1.0, 1.0, "identicall") ;
   nearl (cstub.identicall (10.0), 10.0, 10.0, "identicall") ;
   nearl (cstub.identicall (100.0), 100.0, 100.0, "identicall") ;

   nearl (pi / 6.0, sinl (pi / 6.0), 0.5, "sinl") ;
   near (sin (pi / 6.0), 0.5) ;
   nearl (pi / 3.0, cosl (pi / 3.0), 0.5, "cosl") ;
   near (cos (pi / 3.0), 0.5) ;
   nearl (100.0, log10l (100.0), 2.0, "log10l") ;
   near (log10 (100.0), 2.0) ;

   nearl (1.23456789, log10l (1.23456789), 0.091515, "log10l") ;
   l := 1.23456789 ;
   nearl (l, log10l (l), 0.091515, "log10l") ;

   r := pi / 6.0 ;
   l := pi / 6.0 ;
   nearl (l, sinl (l), 0.5, "sinl") ;
   near (sin (r), 0.5) ;

   r := pi / 3.0 ;
   l := pi / 3.0 ;
   nearl (l, cosl (l), 0.5, "cosl") ;
   near (cos (r), 0.5) ;

   r := 100.0 ;
   l := 100.0 ;
   nearl (l, log10l (l), 2.0, "log10l") ;
   near (log10 (r), 2.0) ;

END testfunc ;


BEGIN
   code := 0 ;
   testfunc ;
   exit (code)
END test.
