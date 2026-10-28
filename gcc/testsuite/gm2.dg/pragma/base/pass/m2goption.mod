(* { dg-do compile } *)
(* { dg-options "-fm2-g" } *)

MODULE m2goption ;

<* TEST $(gm2.option.fm2_g) | ERROR : "the fm2-g option was not set"  *>

BEGIN

END m2goption.
