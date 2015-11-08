with System;

package Init10 is

  type My_Integer is new Integer;
  for My_Integer'Alignment use 1;

  type R1 is record
    I : My_Integer;
  end record;
  for R1'Bit_Order use System.Low_Order_First;
  for R1'Scalar_Storage_Order use System.Low_Order_First;

  type R2 is record
    I : My_Integer;
  end record;
  for R2'Bit_Order use System.High_Order_First;
  for R2'Scalar_Storage_Order use System.High_Order_First;

  My_R1 : constant R1 := (I => 16#12345678#);
  My_R2 : constant R2 := (I => 16#12345678#);

end Init10;
