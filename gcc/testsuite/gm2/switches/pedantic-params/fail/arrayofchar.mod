IMPLEMENTATION MODULE arrayofchar ;

FROM FIO IMPORT WriteChar, WriteLine ;
IMPORT StrLib ;


(*
   Write - writes a string to file f.
*)

PROCEDURE Write (f: File; a: ARRAY OF CHAR) ;
VAR
   len, i: CARDINAL ;
BEGIN
   len := StrLib.StrLen (a) ;
   i := 0 ;
   WHILE i < len DO
      WriteChar (f, a[i]) ;
      INC (i)
   END
END Write ;


PROCEDURE WriteLn (f: File) ;
BEGIN
   WriteLine (f)
END WriteLn ;


END arrayofchar.
