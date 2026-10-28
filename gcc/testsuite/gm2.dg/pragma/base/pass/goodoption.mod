(* { dg-do compile } *)
(* { dg-options "-g" } *)

MODULE goodoption ;

<* TEST $(gm2.option.g) | ERROR : "the g option was not set"  *>

BEGIN

END goodoption.
