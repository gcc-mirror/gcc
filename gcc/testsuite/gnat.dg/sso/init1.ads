with System;

package Init1 is

  type R1 is record
    I : Integer;
  end record;
  for R1'Bit_Order use System.Low_Order_First;
  for R1'Scalar_Storage_Order use System.Low_Order_First;
  for R1 use record
    I at 0 range 0 .. 31;
  end record;

  type R2 is record
    I : Integer;
  end record;
  for R2'Bit_Order use System.High_Order_First;
  for R2'Scalar_Storage_Order use System.High_Order_First;
  for R2 use record
    I at 0 range 0 .. 31;
  end record;

  My_R1 : constant R1 := (I => 16#12345678#);
  My_R2 : constant R2 := (I => 16#12345678#);

end Init1;
