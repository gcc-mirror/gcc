(* { dg-do run } *)
(* { dg-options "-g -fno-scaffold-dynamic" } *)

MODULE hellopim ;  

FROM StrIO IMPORT WriteString, WriteLn ;

BEGIN
   WriteString ("hello world") ; WriteLn
END hellopim.
