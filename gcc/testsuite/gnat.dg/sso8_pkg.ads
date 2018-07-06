with Interfaces;
with System;
with Unchecked_Conversion;

package SSO8_Pkg is

  Val8 : Interfaces.Unsigned_8;

  type Two_Bit_Int is range 0 .. 3;
  for Two_Bit_Int'size use 2;

  type Arr is array (1 .. 5) of Boolean;
  for Arr'scalar_storage_order use System.High_Order_First;
  pragma Pack (Arr);

  type Rec is record
    Boolean_Data : Boolean;
    Array_Data   : Arr;
    Two_Bit_Data : Two_Bit_Int;
  end record;
  for Rec use record
    Boolean_Data at 0 range 0 .. 0;
    Array_Data   at 0 range 1 .. 5;
    Two_Bit_Data at 0 range 6 .. 7;
  end record;
  for Rec'size use 8;
  for Rec'bit_order use System.High_Order_First;
  for Rec'scalar_storage_order use System.High_Order_First;

  function Conv is new Unchecked_Conversion (Rec, Interfaces.Unsigned_8);

end SSO8_Pkg;
