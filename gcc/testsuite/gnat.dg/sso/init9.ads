with Ada.Numerics; use Ada.Numerics;
with System;

package Init9 is

  type R1 is record
    F : Long_Float;
  end record;
  for R1'Bit_Order use System.Low_Order_First;
  for R1'Scalar_Storage_Order use System.Low_Order_First;
  for R1 use record
    F at 0 range 0 .. 63;
  end record;

  type R2 is record
    F : Long_Float;
  end record;
  for R2'Bit_Order use System.High_Order_First;
  for R2'Scalar_Storage_Order use System.High_Order_First;
  for R2 use record
    F at 0 range 0 .. 63;
  end record;

  My_R1 : constant R1 := (F => Pi);
  My_R2 : constant R2 := (F => Pi);

end Init9;
