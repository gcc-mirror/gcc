--  { dg-do run }
--  { dg-options "-gnatws" }

with System;
with Ada.Unchecked_Conversion;

procedure SSO15 is

   type Arr is array (1 .. Integer'Size) of Boolean;
   pragma Pack (Arr);
   for Arr'Scalar_Storage_Order use System.High_Order_First;

   function To_Float is new Ada.Unchecked_Conversion (Arr, Float);
   function To_Int is new Ada.Unchecked_Conversion (Arr, Integer);

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

   A : Arr := (1 .. 2 => True, others => False);

   F1 : Float;
   F2 : R_Float;
   for F2'Address use A'Address;
   pragma Import (Ada, F2);

   I1 : Integer;
   I2 : R_Int;
   for I2'Address use A'Address;
   pragma Import (Ada, I2);

begin
   -- Check that converting to FP yields a big-endian value.
   F1 := To_Float (A);
   if F2.F /= F1 then
      raise Program_Error;
   end if;

   -- Check that converting to integer yields a big-endian value.
   I1 := To_Int (A);
   if I2.I /= I1 then
      raise Program_Error;
   end if;
end;
