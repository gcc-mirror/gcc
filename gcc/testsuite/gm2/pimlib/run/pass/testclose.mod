MODULE testclose ;

IMPORT FIO ;
IMPORT libc ;


(*
   assert -
*)

PROCEDURE assert (condition: BOOLEAN; line: CARDINAL) ;
BEGIN
   IF NOT condition
   THEN
      libc.printf ("%s:%d:assert failed\n", __FILE__, line) ;
      libc.exit (1)
   END
END assert ;


(*
   Init -
*)

PROCEDURE Init ;
VAR
   f: FIO.File ;
BEGIN
   f := FIO.OpenToWrite ('testclose.txt') ;
   assert (FIO.IsNoError (f), __LINE__) ;
   FIO.WriteString (f, 'hello') ;
   assert (FIO.IsNoError (f), __LINE__) ;
   FIO.WriteLine (f) ;
   assert (FIO.IsNoError (f), __LINE__) ;
   assert (FIO.Close (f), __LINE__)
END Init ;


BEGIN
   Init
END testclose.
