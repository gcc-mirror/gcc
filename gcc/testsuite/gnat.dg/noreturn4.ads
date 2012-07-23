package Noreturn4 is

  procedure P1 (Msg : String);
  procedure P1 (Msg : String; Val : Integer);
  pragma No_Return (P1);

  procedure Fatal_Error (X : Integer);
  pragma No_Return (Fatal_Error);

end Noreturn4;
