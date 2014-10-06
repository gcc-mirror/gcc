package body Return4_Pkg  is

  function Get_Value (I : Integer) return Rec is
    Value : Rec := (I1 => I, I2 => I, I3 => I);
  begin
    return Value;
  end;

end Return4_Pkg;
