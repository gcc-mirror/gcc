
(* { dg-do compile } *)
(* { dg-options "-g" } *)

MODULE badmodule ;

IMPORT color ;

PROCEDURE Init ;
BEGIN
   colors.Clear
(* { dg-error "the qualifier 'colors' is unknown, did you mean color?" "colors" { target *-*-* } 11 } *)
END Init ;

BEGIN
   Init
END badmodule.
