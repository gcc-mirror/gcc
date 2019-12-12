--  { dg-do run }

procedure Renaming13 is
   type Stack_Type_Base is array (Natural range <>) of Integer;

   procedure Foo (Buf : in out Stack_Type_Base) is
      S : Stack_Type_Base renames Buf;

      procedure Init is
      begin
         S := (others => 0);
      end;

   begin
      Init;
   end;

   Temp : Stack_Type_Base (1 .. 100);
begin
   Foo (Temp);
end;
