package body Wide_Boolean_Pkg is

   procedure Modify (LH : in out TUINT32; LB : in out TBOOL) is
   begin
      LH := 16#12345678#;
      LB := TRUE;
   end;

end Wide_Boolean_Pkg;
