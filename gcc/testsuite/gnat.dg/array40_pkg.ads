package Array40_Pkg is

  type Rec is record
    I : Integer;
  end record;

  type Arr is array (1 .. 4) of Rec;
  for Arr'Component_Size use 40;

  A : Arr;

end Array40_Pkg;
