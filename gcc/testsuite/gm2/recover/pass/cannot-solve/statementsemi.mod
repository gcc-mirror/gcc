MODULE statementsemi ;

VAR
   x, y: CARDINAL ;
BEGIN
   x := 0 ;
   y := 10 ;
   WHILE x < y DO
      INC (x)  (* missing semicolon  *)
      INC (x)
   END
END statementsemi.