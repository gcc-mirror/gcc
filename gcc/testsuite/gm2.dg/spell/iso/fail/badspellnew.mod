(* { dg-do compile } *)
(* { dg-options "-g" } *)

MODULE badspellnew ;

FROM Storage IMPORT ALLOCATE ;

VAR
   foo: POINTER TO CARDINAL ;
BEGIN
   NEW (Foo)
   (* { dg-error "parameter to NEW must be a pointer, seen unknown, did you mean foo?" "Foo" { target *-*-* } 11 } *)
END badspellnew.
