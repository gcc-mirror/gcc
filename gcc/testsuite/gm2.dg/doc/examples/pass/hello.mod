(* { dg-do run } *)
(* { dg-options "-g -fno-scaffold-dynamic" } *)

MODULE hello ;  

FROM libc IMPORT printf ;

BEGIN
   printf ("hello world\n")
END hello.
