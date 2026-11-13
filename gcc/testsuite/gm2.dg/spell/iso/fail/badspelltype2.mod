
(* { dg-do compile } *)
(* { dg-options "-g -c" } *)

MODULE badspelltype2 ;

TYPE
   Cardinal = CARDINAL ;

VAR
   a: Cardinal ;
   b: Cardina1 ;
 (* { dg-error "unknown symbol 'Cardina1', did you mean Cardinal" "Cardina1" { target *-*-* } 12 } *)
BEGIN
   IF a = b
   THEN
   END
END badspelltype2.
