(* { dg-do compile } *)
(* { dg-options "-g" } *)

MODULE badspellchr2 ;

VAR
   foo: CARDINAL ;
BEGIN
   IF CHR (Foo+1) = 'A'
   (* { dg-error "unknown symbol 'Foo', did you mean foo?" "Foo" { target *-*-* } 9 } *)
   THEN
   END
END badspellchr2.
