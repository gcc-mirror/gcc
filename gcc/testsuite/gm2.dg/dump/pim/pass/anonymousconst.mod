(* { dg-do compile } *)
(* { dg-options "-O0 -fdump-tree-optimized" } *)

MODULE anonymousconst ;
TYPE
   large = SET OF [0..1023] ;
PROCEDURE Check (a: large) ;
END Check ;
BEGIN
   Check(large{2, 3, 4})
END anonymousconst.

(* { dg-final { scan-tree-dump "Check \\(C\\." "optimized" } } *)
