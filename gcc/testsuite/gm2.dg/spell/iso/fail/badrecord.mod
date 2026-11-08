
(* { dg-do compile } *)
(* { dg-options "-g" } *)

MODULE badrecord ;

TYPE
   color = RECORD
              red, blue, green: CARDINAL ;
           END ;

PROCEDURE Init ;
BEGIN
   colors.red := 1 ;
(* { dg-error "the qualifier 'colors' is unknown, did you mean color?" "colors" { target *-*-* } 14 } *)
END Init ;

BEGIN
   Init
END badrecord.
