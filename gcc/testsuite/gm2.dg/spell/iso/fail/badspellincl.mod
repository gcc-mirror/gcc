(* { dg-do compile } *)
(* { dg-options "-g" } *)

MODULE badspellincl ;

VAR
   foo: BITSET ;
BEGIN
   INCL (Foo, 1)
   (* { dg-error "base procedure INCL expects a variable as a parameter, seen unknown, did you mean foo?" "Foo" { target *-*-* } 9 } *)
END badspellincl.
