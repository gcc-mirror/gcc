(* { dg-do compile } *)

MODULE goption ;

<* TEST $(gm2.option.g) & ERROR : "this library should not be compiled with -g"  *>

BEGIN

END goption.
