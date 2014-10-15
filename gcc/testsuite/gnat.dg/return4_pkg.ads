package Return4_Pkg is

  type Rec is record
    I1, I2, I3 : Integer;
  end record;

  function Get_Value (I : Integer) return Rec;

end Return4_Pkg;
