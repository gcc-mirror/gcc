-- { dg-do run }
-- { dg-options "-gnato -O" }

with Interfaces; use Interfaces;

procedure Opt26 is

   procedure Shift_Left_Bool
     (Bool : in Boolean;
      U8 : out Interfaces.Unsigned_8)
   is
   begin
      U8 := Shift_Left (Boolean'Pos (Bool), 6);
   end Shift_Left_Bool;

   procedure Shift_Left_Not_Bool
     (Bool : in Boolean;
      U8 : out Interfaces.Unsigned_8)
   is
   begin
      U8 := Shift_Left (Boolean'Pos (not Bool), 6);
   end Shift_Left_Not_Bool;

   Bool         : constant Boolean := True;
   Byte1, Byte2 : Interfaces.Unsigned_8;

begin

   Shift_Left_Bool (Bool, Byte1);

   Shift_Left_Not_Bool (Bool, Byte2);

   if Byte1 + Byte2 /= 64 then
     raise Program_Error;
   end if;

end;
