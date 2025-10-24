
(* { dg-do compile } *)
(* { dg-options "-g" } *)

MODULE badspellsize ;

VAR
   foo: INTEGER ;
BEGIN
   IF SIZE (Foo) = NIL
   (* { dg-error "SYSTEM procedure SIZE expects a variable or type as its parameter, seen unknown, did you mean foo?" "Foo" { target *-*-* } 10 } *)
   THEN
   END
END badspellsize.
