(* { dg-skip-if "" { *-*-* } }  *)

MODULE badimp ;

(* User forgot the IMPLEMENTATION keyword prior to MODULE.  *)

BEGIN
END badimp.
