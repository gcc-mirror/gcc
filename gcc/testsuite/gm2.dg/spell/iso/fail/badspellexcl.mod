(* { dg-do compile } *)
(* { dg-options "-g" } *)

MODULE badspellexcl ;

VAR
   foo: BITSET ;
BEGIN
   EXCL (Foo, 1)
   (* { dg-error "base procedure EXCL expects a variable as a parameter, seen unknown, did you mean foo?" "Foo" { target *-*-* } 9 } *)
END badspellexcl.
