with System;

package SSO2 is

  type Arr1 is array (1 .. 4) of Character;
  for Arr1'Scalar_Storage_Order use System.High_Order_First;

  type Arr2 is array (1 .. 4) of Character;
  for Arr2'Scalar_Storage_Order use System.Low_Order_First;

  procedure Proc (A1 : Arr1; A2 : out Arr2);

end SSO2;
