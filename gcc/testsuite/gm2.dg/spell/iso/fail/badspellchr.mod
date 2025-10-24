(* { dg-do compile } *)
(* { dg-options "-g" } *)

MODULE badspellchr ;

VAR
   foo: CARDINAL ;
BEGIN
   IF CHR (Foo) = 'A'
   (* { dg-error "the parameter to CHR must be a variable or constant, seen 'Foo', did you mean foo?" "Foo" { target *-*-* } 9 } *)
   THEN
   END
END badspellchr.
