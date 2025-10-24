(* { dg-do compile } *)
(* { dg-options "-g" } *)

MODULE badspellcap ;

VAR
   foo: CHAR ;
BEGIN
   IF CAP (Foo) = 'A'
   (* { dg-error "the parameter to CAP must be a variable or constant, seen 'Foo', did you mean foo?" "Foo" { target *-*-* } 9 } *)
   THEN
   END
END badspellcap.
