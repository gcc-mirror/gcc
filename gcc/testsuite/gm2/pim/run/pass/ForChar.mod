MODULE ForChar ;

FROM StrLib IMPORT StrEqual ;
FROM libc IMPORT printf, exit ;


(*
   Test -
*)

PROCEDURE Test ;
VAR
   ch    : CHAR ;
   digits: ARRAY [0..10] OF CHAR ;
   c     : CARDINAL ;
BEGIN
   c := 0 ;
   FOR ch := '0' TO '9' DO
      digits[c] := ch ;
      INC (c)
   END ;
   digits[10] := 0C ;
   IF NOT StrEqual (digits, "0123456789")
   THEN
      printf ("digits should equal 0123456789, but is %s\n", digits) ;
      exit (1)
   END
END Test ;


BEGIN
   Test
END ForChar.
