with System;

package Init7 is

  type Nested1 is record
    C1 : Integer;
    C2 : Integer;
    C3 : Integer;
  end record;
  for Nested1'Bit_Order use System.Low_Order_First;
  for Nested1'Scalar_Storage_Order use System.Low_Order_First;
  for Nested1 use record
    C1 at 0 range 0 .. 31;
    C2 at 4 range 0 .. 31;
    C3 at 8 range 0 .. 31;
  end record;

  type R1 is record
    I : Integer;
    N : Nested1;
  end record;
  for R1'Bit_Order use System.Low_Order_First;
  for R1'Scalar_Storage_Order use System.Low_Order_First;
  for R1 use record
    I at 0 range 0 .. 31;
    N at 4 range 0 .. 95;
  end record;

  type Nested2 is record
    C1 : Integer;
    C2 : Integer;
    C3 : Integer;
  end record;
  for Nested2'Bit_Order use System.High_Order_First;
  for Nested2'Scalar_Storage_Order use System.High_Order_First;
  for Nested2 use record
    C1 at 0 range 0 .. 31;
    C2 at 4 range 0 .. 31;
    C3 at 8 range 0 .. 31;
  end record;

  type R2 is record
    I : Integer;
    N : Nested2;
  end record;
  for R2'Bit_Order use System.High_Order_First;
  for R2'Scalar_Storage_Order use System.High_Order_First;
  for R2 use record
    I at 0 range 0 .. 31;
    N at 4 range 0 .. 95;
  end record;

  My_R1 : constant R1 := (I => 16#12345678#,
                          N => (16#AB0012#, 16#CD0034#, 16#EF0056#));

  My_R2 : constant R2 := (I => 16#12345678#,
                          N => (16#AB0012#, 16#CD0034#, 16#EF0056#));

end Init7;
