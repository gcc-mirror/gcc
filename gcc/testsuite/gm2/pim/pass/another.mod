MODULE another ;

TYPE
   MYSHORTREAL = REAL;
   
TYPE
   PROCA = PROCEDURE (VAR ARRAY OF REAL);
   PROCB = PROCEDURE (VAR ARRAY OF MYSHORTREAL);

VAR
   pa: PROCA; pb: PROCB;
   x: ARRAY [0..1] OF REAL;
   y: ARRAY [0..1] OF MYSHORTREAL;
   
PROCEDURE ProcA(VAR z: ARRAY OF REAL);
BEGIN
END ProcA ;

PROCEDURE ProcB(VAR z: ARRAY OF MYSHORTREAL);
BEGIN
END ProcB ;

BEGIN
   x := y;
   pa := ProcA;
   pb := ProcB;
   pa(x);
   pa(y);
   pb(x);
   pb(y);
   pa := ProcB;
   pb := ProcA;
   pa(x);
   pa(y);
   pb(x);
   pb(y)
END another.
