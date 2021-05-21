package body Derived_Type7 is

  procedure Proc (Size : Natural) is
    type Sar_Six_Bit_Arr is new Six_Bit_Data_Array_Type (1 .. Size);
  begin
    null;
  end;

end Derived_Type7;
