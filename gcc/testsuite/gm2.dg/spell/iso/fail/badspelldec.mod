(* { dg-do compile } *)
(* { dg-options "-g" } *)

MODULE badspelldec ;

VAR
   foo: CARDINAL ;
BEGIN
   DEC (Foo)
   (* { dg-error "base procedure DEC expects a variable as a parameter but was given unknown, did you mean foo?" "Foo" { target *-*-* } 9 } *)
END badspelldec.
