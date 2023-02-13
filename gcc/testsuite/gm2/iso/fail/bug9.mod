MODULE bug9 ;

(* Missing import; Using the element in an expression crashes the compiler. *)
(* Using the element in an assignment is working. *)

FROM ChanConsts IMPORT (*read, text,*) FlagSet;

VAR
   flags: FlagSet;
BEGIN
   flags := read ;

END bug9.
