-- { dg-do compile }

with System;

package SSO1 is

   type R is record  -- { dg-error "inconsistent with bit order" }
      B : Boolean;
   end record;
   for R'Bit_Order use System.Low_Order_First;
   for R'Scalar_Storage_Order use System.High_Order_First;  -- { dg-warning "no component clause" }

   type RW is record
      B : Boolean;
   end record;
   for RW'Bit_Order use System.Low_Order_First;
   for RW'Scalar_Storage_Order use System.Low_Order_First;  -- { dg-warning "no component clause" }

end SSO1;
