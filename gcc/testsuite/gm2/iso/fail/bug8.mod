MODULE bug8;

(* Missing import; Using the element in an expression crashes the compiler. *)
(* Using the element in an assignment is working. *)

FROM ChanConsts IMPORT (*read, text,*) FlagSet;

VAR flags:FlagSet;
BEGIN
    flags:=read; (* OK, no expression *)
    flags:=read+text; (* gm2 -g  -fiso Bug8.mod
Bug8.mod:10:5:*** fatal error ***
../../gm2/gcc-versionno/gcc/gm2/gm2-compiler/SymbolTable.mod:6399:1:*** internal error *** illegal symbol
cc1gm2: internal compiler error: Aborted
*)

END bug8.
