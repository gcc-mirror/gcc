IMPLEMENTATION MODULE incsubrange ;


PROCEDURE incMonth (VAR m: Month) ;
BEGIN
   m := m + 1
END incMonth ;


PROCEDURE incMonth2 (VAR m: Month) ;
BEGIN
   m := getMonth () + 1
END incMonth2 ;


(*
   getMonth -
*)

PROCEDURE getMonth () : CARDINAL ;
BEGIN
   RETURN 1
END getMonth ;


END incsubrange.
