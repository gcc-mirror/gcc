
(* { dg-do compile } *)
(* { dg-options "-g -c" } *)

MODULE valspell2 ;

TYPE
   i1 = CHAR ;
VAR
   c: CARDINAL ;
   i9: INTEGER ;
BEGIN
   i9 := 1 ;
   c := VAL (CARDINAL, ii)
   (* { dg-error "undeclared variable or constant expression found in builtin procedure function VAL 'ii', did you mean i9" "ii" { target *-*-* } 14 } *)
END valspell2.
