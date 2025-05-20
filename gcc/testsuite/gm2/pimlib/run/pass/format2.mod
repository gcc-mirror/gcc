MODULE format2;

FROM libc IMPORT exit, printf ;
FROM Terminal IMPORT Write, WriteLn;
FROM NumberIO IMPORT WriteCard;
FROM DynamicStrings IMPORT String, Length, char, InitString;
FROM FormatStrings IMPORT Sprintf1;

PROCEDURE WriteString (s: String);
VAR
   l, i: CARDINAL;
BEGIN
   l := Length (s) ;
   i := 0 ;
   WHILE i < l DO
      Write (char (s, i)) ;
      INC (i)
   END
END WriteString;


(*
   assert -
*)

PROCEDURE assert (cond: BOOLEAN; line: CARDINAL; file: ARRAY OF CHAR) ;
BEGIN
   IF NOT cond
   THEN
      printf ("%s:%d assertion failed\n", file, line);
      exit (1)
   END
END assert ;


VAR
   n: CARDINAL;
   r, s: String;
BEGIN
   n := 2;
   r := InitString("%u pieces of cake") ;
   WriteString (r) ; WriteLn ;
   assert (Length (r) = 17, __LINE__, __FILE__) ;
   s := Sprintf1 (r, n) ;
   WriteCard (Length (s), 4) ; WriteLn ;
   assert (Length (s) = 16, __LINE__, __FILE__) ;

   r := InitString("%d pieces of cake") ;
   WriteString (r) ; WriteLn ;
   assert (Length (r) = 17, __LINE__, __FILE__) ;
   s := Sprintf1 (r, n) ;
   WriteCard (Length (s), 4) ; WriteLn ;
   assert (Length (s) = 16, __LINE__, __FILE__) ;

   r := InitString("%x pieces of cake") ;
   WriteString (r) ; WriteLn ;
   assert (Length (r) = 17, __LINE__, __FILE__) ;
   s := Sprintf1 (r, n) ;
   WriteCard (Length (s), 4) ; WriteLn ;
   assert (Length (s) = 16, __LINE__, __FILE__) ;

   WriteString (InitString ('all tests pass')) ; WriteLn ;
END format2.
