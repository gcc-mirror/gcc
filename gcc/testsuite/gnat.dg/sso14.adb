--  { dg-do run }
--  { dg-options "-gnatws" }

with System;
with Ada.Unchecked_Conversion;

procedure SSO14 is

   type Arr is array (1 .. Integer'Size) of Boolean;
   pragma Pack (Arr);
   for Arr'Scalar_Storage_Order use System.High_Order_First;

   function From_Float is new Ada.Unchecked_Conversion (Float, Arr);
   function From_Int is new Ada.Unchecked_Conversion (Integer, Arr);

   type R_Float is record
     F : Float;
   end record;
   for R_Float'Bit_Order use System.High_Order_First;
   for R_Float'Scalar_Storage_Order use System.High_Order_First;

   type R_Int is record
     I : Integer;
   end record;
   for R_Int'Bit_Order use System.High_Order_First;
   for R_Int'Scalar_Storage_Order use System.High_Order_First;

   F1 : Float := 1.234567;
   FA : Arr;
   F2 : R_Float;
   for F2'Address use FA'Address;
   pragma Import (Ada, F2);

   I1 : Integer := 1234567;
   IA : Arr;
   I2 : R_Int;
   for I2'Address use IA'Address;
   pragma Import (Ada, I2);

begin
   -- Check that converting a FP value yields a big-endian array
   FA := From_Float (F1);
   if F2.F /= F1 then
      raise Program_Error;
   end if;

   -- Check that converting an integer value yields a big-endian array.
   IA := From_Int (I1);
   if I2.I /= I1 then
      raise Program_Error;
   end if;
end;
