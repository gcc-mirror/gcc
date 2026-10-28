(* { dg-do compile } *)
(* { dg-options "-g" } *)

MODULE mistyped ;

<* TEST ?(gm2.default.pim2) | FATAL *>

(* User mistyped pim2 and should have type fpim2.  *)

BEGIN

END mistyped.
