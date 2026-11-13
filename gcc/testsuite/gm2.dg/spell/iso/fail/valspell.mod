
(* { dg-do compile } *)
(* { dg-options "-g -c" } *)

MODULE valspell ;

CONST
   CARDINAl = 1 ;
VAR
   c: CARDINAL ;
   i: INTEGER ;
BEGIN
   i := 1 ;
   c := VAL (CARDIN, i)
   (* { dg-error "undeclared type found in builtin procedure function VAL 'CARDIN', did you mean CARDINAL" "CARDIN" { target *-*-* } 14 } *)
END valspell.
