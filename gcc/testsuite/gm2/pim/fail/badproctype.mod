MODULE badproctype ;

TYPE
   PROCA = PROCEDURE (VAR ARRAY OF REAL);
   PROCB = PROCEDURE (VAR ARRAY OF SHORTREAL);

VAR
   pa: PROCA; pb: PROCB;
   x: ARRAY [0..1] OF REAL;
   y: ARRAY [0..1] OF SHORTREAL;

PROCEDURE ProcA(VAR z: ARRAY OF REAL);
BEGIN
END ProcA ;

PROCEDURE ProcB(VAR z: ARRAY OF SHORTREAL);
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
   pa := ProcB;  (* proctype does not match.  *)
   pb := ProcA;  (* proctype does not match.  *)
   pa(x);
   pa(y);
   pb(x);
   pb(y)
END badproctype.
