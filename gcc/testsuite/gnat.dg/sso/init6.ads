with System;

package Init6 is

  type Arr1 is array (1 .. 3) of Integer;
  for Arr1'Scalar_Storage_Order use System.High_Order_First;

  type R1 is record
    I : Integer;
    A : Arr1;
  end record;
  for R1'Bit_Order use System.Low_Order_First;
  for R1'Scalar_Storage_Order use System.Low_Order_First;
  for R1 use record
    I at 0 range 0 .. 31;
    A at 4 range 0 .. 95;
  end record;

  type Arr2 is array (1 .. 3) of Integer;
  for Arr2'Scalar_Storage_Order use System.Low_Order_First;

  type R2 is record
    I : Integer;
    A : Arr2;
  end record;
  for R2'Bit_Order use System.High_Order_First;
  for R2'Scalar_Storage_Order use System.High_Order_First;
  for R2 use record
    I at 0 range 0 .. 31;
    A at 4 range 0 .. 95;
  end record;

  My_R1 : constant R1 := (I => 16#12345678#,
                          A => (16#AB0012#, 16#CD0034#, 16#EF0056#));

  My_R2 : constant R2 := (I => 16#12345678#,
                          A => (16#AB0012#, 16#CD0034#, 16#EF0056#));

end Init6;
