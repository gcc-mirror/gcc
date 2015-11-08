-- { dg-do run }

with System;
with Ada.Unchecked_Conversion;
with Ada.Streams; use Ada.Streams;
with Ada.Text_IO;

procedure SSO1 is

   type Unsigned_Integer_4 is mod 2 ** 32;
   for Unsigned_Integer_4'Size use 32;

   Default_Bit_Order_Pos : constant Natural := System.Bit_Order'Pos (System.Default_Bit_Order);

   Opposite_Bit_Order_Pos : constant Natural := 1 - Default_Bit_Order_Pos;

   Opposite_Bit_Order : constant System.Bit_Order := System.Bit_Order'Val (Opposite_Bit_Order_Pos);

   type Rec is
      record
	 X, Y : Unsigned_Integer_4;
      end record;
   for Rec'Bit_Order use System.Default_Bit_Order;
   for Rec'Scalar_Storage_Order use System.Default_Bit_Order;

   for Rec use
      record
	 X at 0 * 4 range 0 .. 31;
	 Y at 1 * 4 range 0 .. 31;
      end record;
   
   type Nested_Rec is
      record 
	 I : Unsigned_Integer_4;
	 R : Rec;
	 J : Unsigned_Integer_4;
      end record;
   for Nested_Rec use
      record
	 I at 0 * 4 range 0 .. 31;
	 R at 1 * 4 range 0 .. 63;
	 J at 3 * 4 range 0 .. 31;
	 end record;

   for Nested_Rec'Bit_Order use Opposite_Bit_Order;
   for Nested_Rec'Scalar_Storage_Order use Opposite_Bit_Order;

   Nr : Nested_Rec 
     := (I => 1,
	 R => (X => 1,
	       Y => 1),
	 J => 1);
   
   subtype Nested_Rec_As_Stream is Ada.Streams.Stream_Element_Array (1 ..16);

   function To_Stream is
     new Ada.Unchecked_Conversion (Nested_Rec, Nested_Rec_As_Stream);

   Nr_Stream : constant Nested_Rec_As_Stream := To_Stream (Nr);

   Expected : constant array (System.Bit_Order) of Nested_Rec_As_Stream :=
                (System.Low_Order_First =>
                   (0, 0, 0, 1,
                    1, 0, 0, 0,
                    1, 0, 0, 0,
                    0, 0, 0, 1),
                 System.High_Order_First =>
                   (1, 0, 0, 0,
                    0, 0, 0, 1,
                    0, 0, 0, 1,
                    1, 0, 0, 0));

begin
   if Nr_Stream /= Expected (System.Default_Bit_Order) then
      raise Program_Error;
   end if;
end;
