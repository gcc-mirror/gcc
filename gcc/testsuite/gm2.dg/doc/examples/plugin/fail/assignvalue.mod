(* { dg-do compile } *)
(* { dg-options "-fsoft-check-all -fm2-plugin" } *)
(* { dg-skip-if "" { *-*-* }  { "*" } { "-O2" } } *)

MODULE assignvalue ;  (*!m2iso+gm2*)

PROCEDURE bad () : INTEGER ;
VAR
   i: INTEGER ;
BEGIN
   i := -1 ;
   RETURN i
END bad ;

VAR
   foo: CARDINAL ;
BEGIN
   (* The m2rte plugin will detect this as an error, post
      optimization.  *)
   foo := bad ()  (* { dg-error "error: In program module assignvalue" } *)
   (* { dg-begin-multiline-output "" }
runtime error will occur, assignment will cause a range error, as the runtime instance value of 'CARDINAL' does not overlap with the type 'INTEGER'
     { dg-end-multiline-output "" } *)

END assignvalue.
