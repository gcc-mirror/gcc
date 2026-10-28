MODULE divmodpragma2 ;

(* A test module to test whether the dialect can be changed within a module.  *)

<* gm2.default.fm2_whole_program=TRUE *>
<* gm2.default.fm2_g=TRUE *>
<* gm2.default.fsoft_check_all=TRUE *>
<* gm2.default.fpim2=TRUE *>    (* Default to the pim2 dialect.  *)
<* gm2.default.fpim3=FALSE *>    (* Default off for pim3 dialect.  *)
<* gm2.default.fpim4=FALSE *>    (* Default off for pim4 dialect.  *)


(*
   pim2 -
*)

PROCEDURE pim2 ;
<* TEST $(gm2.default.fpim2)
   | ERROR : "fpim2 should be set by default."  *>
<* TEST $(gm2.default.fpim2) | ERROR  *>
BEGIN
END pim2 ;


(*
   pim4 -
*)

PROCEDURE pim4 ;
<* gm2.local.fpim2=FALSE *>
<* gm2.local.fpim4=TRUE *>
<* TEST $(gm2.local.fpim2)
   & ERROR : "fpim2 should not be set now."  *>
<* TEST $(gm2.local.fpim4)
   | ERROR : "fpim4 should be set now."  *>
BEGIN
END pim4 ;


(*
   pim3 -
*)

PROCEDURE pim3 ;
<* gm2.local.fpim2=FALSE *>
<* gm2.local.fpim3=TRUE *>
<* TEST $(gm2.local.fpim2)
   & ERROR : "fpim2 should not be set now."  *>
<* TEST $(gm2.local.fpim4)
   & ERROR : "fpim4 should not be set now."  *>
BEGIN
END pim3 ;


BEGIN
   pim2 ;
   pim3 ;
   pim4
END divmodpragma2.
