-- { dg-do compile }

package Derived_Type7 is

  type Six_Bit_Data_Type is range 0 .. 63;
  for Six_Bit_Data_Type'Size use 6;

  type Six_Bit_Data_Array_Type is array (Integer range <>) of Six_Bit_Data_Type;
  for Six_Bit_Data_Array_Type'Component_Size use 6;

  procedure Proc (Size : Natural);

end Derived_Type7;
