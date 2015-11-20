with System;

package Init12 is

  type Arr1 is array (1 .. 3) of Integer;
  for Arr1'Scalar_Storage_Order use System.Low_Order_First;

  type Arr11 is array (1 .. 2, 1 .. 2) of Integer;
  for Arr11'Scalar_Storage_Order use System.Low_Order_First;

  type Arr2 is array (1 .. 3) of Integer;
  for Arr2'Scalar_Storage_Order use System.High_Order_First;

  type Arr22 is array (1 .. 2, 1 .. 2) of Integer;
  for Arr22'Scalar_Storage_Order use System.High_Order_First;

  My_A1   : constant Arr1   := (16#AB0012#, 16#CD0034#, 16#EF0056#);
  My_A11  : constant Arr11  := (1 => (16#AB0012#, 16#CD0034#),
                                2 => (16#AB0012#, 16#CD0034#));

  My_A2   : constant Arr2   := (16#AB0012#, 16#CD0034#, 16#EF0056#);
  My_A22  : constant Arr22  := (1 => (16#AB0012#, 16#CD0034#),
                                2 => (16#AB0012#, 16#CD0034#));

end Init12;
