
(* { dg-do compile } *)
(* { dg-options "-g -c" } *)

MODULE valspell3 ;

TYPE
   i1 = CHAR ;
VAR
   c: CARDINAL ;
   i9: INTEGER ;
BEGIN
   i9 := 1 ;
   c := VAL (CARDINAL, i0)
   (* { dg-error "undeclared variable or constant expression found in builtin procedure function VAL 'i0', did you mean i9" "i0" { target *-*-* } 14 } *)
END valspell3.
