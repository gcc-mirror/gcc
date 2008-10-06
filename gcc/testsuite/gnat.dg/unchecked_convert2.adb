-- { dg-do run }

with Ada.Unchecked_Conversion;
with Ada.Streams; use Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;

procedure Unchecked_Convert2 is

   subtype Day_Number is Integer range 0 .. 31;

   subtype Byte_Array_Of_Integer is Stream_Element_Array
     (1 .. Integer'Size / Stream_Element_Array'Component_Size);

   function To_Byte_Array is
      new Ada.Unchecked_Conversion (Integer, Byte_Array_Of_Integer);

   Day_Now : Day_Number;
   Pragma Volatile (Day_Now);

   Arr : Stream_Element_Array (1 .. 12) := (others => 16#ff#);

   procedure Test (Arr : Stream_Element_Array) is
   begin
      if Arr(5) /= 0 or Arr(6) /= 0 or Arr(7) /= 0 or Arr(8) /= 0 then
         raise Program_Error;
      end if;
   end;

begin
   Day_Now := 0;
   Arr (5 .. 8) := To_Byte_Array (Day_Now);
   Test (Arr);
   Arr (1) := 16#ff#;
end Unchecked_Convert2;
