(* { dg-do compile } *)
(* { dg-options "-g" } *)

MODULE hello2 ;

FROM libc IMPORT printf ;

PROCEDURE foo ;
BEGIN
   printf ("hello world\n")
END foo ;

BEGIN
   foo
END hello2.
