
(* { dg-do compile } *)
(* { dg-options "-g" } *)

MODULE badspelladr ;

FROM SYSTEM IMPORT ADR ;

VAR
   foo: INTEGER ;
BEGIN
   IF ADR (Foo) = NIL
   (* { dg-error "SYSTEM procedure ADR expects a variable, procedure or a constant string as its parameter, seen unknown, did you mean foo?" "Foo" { target *-*-* } 12 } *)
   THEN
   END
END badspelladr.
