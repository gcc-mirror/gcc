with System;

package SSO9_Pkg is

   type Rec (D : Boolean := False) is record
      B : Boolean;
   end record;

   for Rec'Bit_Order use System.High_Order_First;
   for Rec'Scalar_Storage_Order use System.High_Order_First;

   type Arr is array (1 .. 16) of Rec;

   procedure Proc (A : Arr);

end SSO9_Pkg;
