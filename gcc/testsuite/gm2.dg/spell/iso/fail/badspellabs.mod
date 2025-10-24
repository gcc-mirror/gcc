
(* { dg-do compile } *)
(* { dg-options "-g" } *)

MODULE badspellabs ;

VAR
   foo: INTEGER ;
BEGIN
   IF ABS (Foo) = 1
   (* { dg-error "the parameter to ABS must be a variable or constant, seen 'Foo', did you mean foo?" "Foo" { target *-*-* } 10 } *)
   THEN
   END
END badspellabs.
