with Ada.Unchecked_Conversion;

package body Inline2_Pkg is

   type Ieee_Short_Real is
      record
         Mantisse_Sign : Integer range 0 .. 1;
         Exponent      : Integer range 0 .. 2 **  8 - 1;
         Mantisse      : Integer range 0 .. 2 ** 23 - 1;
      end record;

   for Ieee_Short_Real use
      record
         Mantisse_Sign at 0 range 31 .. 31;
         Exponent      at 0 range 23 .. 30;
         Mantisse      at 0 range  0 .. 22;
      end record;

   function Valid_Real (Number : Float) return Boolean is
      function To_Ieee_Short_Real is
         new Ada.Unchecked_Conversion (Float, Ieee_Short_Real);
   begin
      return To_Ieee_Short_Real (Number).Exponent /= 255;
   end Valid_Real;

   function Invalid_Real return Float is
      function To_Float is
         new Ada.Unchecked_Conversion (Ieee_Short_Real, Float);
   begin
      return To_Float (Ieee_Short_Real'(Mantisse_Sign => 0,
                                        Exponent => 255, Mantisse => 0));
   end Invalid_Real;

end Inline2_Pkg;
