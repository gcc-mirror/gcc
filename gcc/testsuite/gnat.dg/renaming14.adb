--  { dg-do run }

procedure Renaming14 is
   type Rec_Typ is record
      XX : Integer;
   end record;

   type Stack_Type_Base is array (Natural range <>) of Rec_Typ;

   generic
      S : in out Stack_Type_Base;
   package Stack is
      procedure Init;
   end;

   package body Stack is
      procedure Init is
      begin
         S := (others => (XX => 0));
      end;
   end;

   procedure Foo (Buf : in out Stack_Type_Base) is
      package Stack_Inst is new Stack (Buf);
   begin
      Stack_Inst.Init;
   end;

   Temp : Stack_Type_Base (1 .. 100);
begin
   Foo (Temp);
end;
