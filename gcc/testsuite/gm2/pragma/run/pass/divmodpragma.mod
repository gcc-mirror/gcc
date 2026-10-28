MODULE divmodpragma ;

(* A test module to test whether the dialect can be changed within a module.  *)

(* <* gm2.local.fm2_whole_program=TRUE *> *)
<* gm2.local.fm2_g=TRUE *>
<* gm2.local.fsoft_check_all=TRUE *>
<* gm2.local.fpim4=FALSE *>   (* Turn off the default pim4 dialect.  *)
<* gm2.local.fpim2=TRUE *>    (* Default to the pim2 dialect.  *)
<* gm2.local.fpositive_mod_floor_div=FALSE *>
(* Turn off positive_mod_floor_div.  *)

FROM libc IMPORT printf ;



(*
   Assert -
*)

PROCEDURE Assert (condition: BOOLEAN; code: CARDINAL) ;
BEGIN
   IF NOT condition
   THEN
      printf ("test %d failed\n") ;
      HALT (code)
   END
END Assert ;


(*
   pim2 -
*)

PROCEDURE pim2 ;
<* gm2.local.fpim4=FALSE *>   (* Turn off the default pim4 dialect.  *)
<* TEST $(gm2.local.fpim2)
   | ERROR : "fpim2 should be set."  *>
VAR
   result,
   i, j  : INTEGER ;
BEGIN
   i := -31 ;
   j := -10 ;
   result := i DIV j ;
   Assert (result = 3, 2)
END pim2 ;


(*
   pim4 -
*)

PROCEDURE pim4 ;
<* gm2.local.fpim2=FALSE *>
<* gm2.local.fpim4=TRUE *>
<* TEST $(gm2.local.fpim2)
   & ERROR : "fpim2 should not be be set now."  *>
VAR
   result,
   i, j  : INTEGER ;
BEGIN
   i := -31 ;
   j := -10 ;
   result := i DIV j ;
   Assert (result = 4, 4)
END pim4 ;


(*
   pim3 -
*)

PROCEDURE pim3 ;
<* gm2.local.fpim2=FALSE *>
<* gm2.local.fpim3=TRUE *>
<* TEST $(gm2.local.fpim2)
   & ERROR : "fpim2 should not be be set now."  *>
<* TEST $(gm2.local.fpim4)
   & ERROR : "fpim4 should not be be set now."  *>
VAR
   result,
   i, j  : INTEGER ;
BEGIN
   i := -31 ;
   j := -10 ;
   result := i DIV j ;
   Assert (result = 3, 3)
END pim3 ;


BEGIN
   pim2 ;
   pim3 ;
   pim4 ;
   printf ("all tests passed\n")
END divmodpragma.
