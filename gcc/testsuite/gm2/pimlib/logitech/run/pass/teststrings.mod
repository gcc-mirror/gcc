MODULE teststrings ;

IMPORT InOut,Strings;

VAR
   content : ARRAY[1..256] OF CHAR;
   position: CARDINAL;

(* the content is just random text.  *)

BEGIN
   content := "erreur: In program module « essai3 »: attempting to pass (1) parameters to procedure";
   InOut.WriteString(content);
   InOut.WriteLn;
   position := Strings.Pos ("IMPORT", content);
END teststrings .
