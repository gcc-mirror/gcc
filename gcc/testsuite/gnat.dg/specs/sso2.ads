-- { dg-do compile }
-- { dg-options "-gnatws" }

with System;

package SSO2 is

  I : Integer;

  type Rec1 is record
    A : System.Address;
  end record;
  for Rec1'Bit_Order use System.High_Order_First;
  for Rec1'Scalar_Storage_Order use System.High_Order_First;

  R1 : Rec1 := (A => I'Address);

  type Rec2 is record
    A : System.Address;
  end record;
  for Rec2'Bit_Order use System.Low_Order_First;
  for Rec2'Scalar_Storage_Order use System.Low_Order_First;

  R2 : Rec2 := (A => I'Address);

end SSO2;
