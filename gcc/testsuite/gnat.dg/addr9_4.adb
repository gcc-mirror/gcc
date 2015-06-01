-- { dg-do compile }

with Ada.Streams; use Ada.Streams;

procedure Addr9_4 is

   type Signal_Type is mod 2 ** 16;

   type A_Item is record
      I : Signal_Type;
      Q : Signal_Type;
   end record
   with Size => 32;

   for A_Item use record
      I at 0 range 0 .. 15;
      Q at 2 range 0 .. 15;
   end record;

   type A_Array_Type is
     array (Positive range <>)
     of A_Item
   with Alignment => 16;

   pragma Pack (A_Array_Type);

   type B_Array_Type is new Ada.Streams.Stream_Element_Array
   with Alignment => 16;

   Ct_Count : constant := 7_000;

   package Set is
      A : A_Array_Type := (1 .. Ct_Count => <>);
      B : B_Array_Type := (1 .. Ct_Count * A_Item'Size / 8 => <>);
      for B'Address use A'Address;
   end Set;

begin
   null;
end;
