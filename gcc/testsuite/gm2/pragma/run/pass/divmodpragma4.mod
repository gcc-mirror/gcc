MODULE divmodpragma4 ;

(* A test module to test whether the dialect can be changed within a module.  *)

<* gm2.local.fm2_g=TRUE *>
<* gm2.local.fpim2=TRUE *>    (* Default to the pim2 dialect.  *)


(*
   pim4 -
*)

PROCEDURE pim4 ;
<* gm2.local.fpim2=FALSE *>
<* gm2.local.fpim4=TRUE *>
<* TEST $(gm2.local.fpim2)
   & ERROR : "fpim2 should not be be set now."  *>
BEGIN
END pim4 ;


BEGIN
   pim4
END divmodpragma4.
