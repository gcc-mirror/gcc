
(* { dg-do compile } *)
(* { dg-options "-g -c" } *)

MODULE badspelltype ;

VAR
   a: CARDINAL ;
   b: CARDINAl ;
 (* { dg-error "unknown symbol 'CARDINAl', did you mean CARDINAL" "CARDINAl" { target *-*-* } 9 } *)
BEGIN
   IF a = b
   THEN
   END
END badspelltype.
