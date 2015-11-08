with System;

package Init2 is

  type Small is mod 2**2;
  for Small'Size use 2;

  type Count is mod 2**9;
  for Count'Size use 9;

  type Arr1 is array (1 .. 3) of Count;
  pragma Pack (Arr1);
  for Arr1'Size use 27;
  for Arr1'Scalar_Storage_Order use System.Low_Order_First;

  type R1 is record
    S1 : Small;
    I  : Integer;
    S2 : Small;
    A  : Arr1;
    B  : Boolean;
  end record;
  for R1'Bit_Order use System.Low_Order_First;
  for R1'Scalar_Storage_Order use System.Low_Order_First;
  for R1 use record
    S1 at 0 range  0 ..  1;
    I  at 0 range  2 .. 33;
    S2 at 0 range 34 .. 35;
    A  at 0 range 36 .. 62;
    B  at 0 range 63 .. 63;
  end record;
  for R1'Size use 64;

  type Arr2 is array (1 .. 3) of Count;
  pragma Pack (Arr2);
  for Arr2'Size use 27;
  for Arr2'Scalar_Storage_Order use System.High_Order_First;

  type R2 is record
    S1 : Small;
    I  : Integer;
    S2 : Small;
    A  : Arr2;
    B  : Boolean;
  end record;
  for R2'Bit_Order use System.High_Order_First;
  for R2'Scalar_Storage_Order use System.High_Order_First;
  for R2 use record
    S1 at 0 range  0 ..  1;
    I  at 0 range  2 .. 33;
    S2 at 0 range 34 .. 35;
    A  at 0 range 36 .. 62;
    B  at 0 range 63 .. 63;
  end record;
  for R2'Size use 64;

  My_R1 : constant R1 := (S1 => 2,
                          I  => 16#12345678#,
                          S2 => 1,
                          A  => (16#AB#, 16#CD#, 16#EF#),
                          B  => True);

  My_R2 : constant R2 := (S1 => 2,
                          I  => 16#12345678#,
                          S2 => 1,
                          A  => (16#AB#, 16#CD#, 16#EF#),
                          B  => True);

end Init2;
