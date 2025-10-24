(* { dg-do compile } *)
(* { dg-options "-g" } *)

MODULE badspellinc ;

VAR
   foo: CARDINAL ;
BEGIN
   INC (Foo)
   (* { dg-error "base procedure INC expects a variable as a parameter but was given unknown, did you mean foo?" "Foo" { target *-*-* } 9 } *)

END badspellinc.
