(* { dg-do compile } *)
(* { dg-options "-g" } *)

MODULE constaggregate ;

TYPE
   callType = RECORD
                 callNick  : ARRAY [0..8] OF CHAR;
                 callString: ARRAY [0..20] OF CHAR;
              END ;

CONST
   cls = CallType {"nickname", "realname"} ;
   (* { dg-error "constructor type 'CallType' is undefined, did you mean callType?" "CallType" { target *-*-* } 13 } *)
   (* { dg-error "the type of the constant declaration 'cls' cannot be determined" "cls" { target *-*-* } 13 } *)


BEGIN
END constaggregate.
