(* { dg-do compile } *)
(* { dg-options "-g" } *)

MODULE fatal2 ;  (*!m2iso+gm2*)

<* MSG = FATAL "a user generated warning message"  *>

BEGIN
   xyz
END fatal2.
