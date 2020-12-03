package Opt90b_Pkg is

  type Rec is record
    A : Short_Short_Integer;
    B : Integer;
    C : Short_Integer;
    D : String (1 .. 12);
  end record;
  pragma Pack (Rec);
  for Rec'Alignment use 1;

  type Data is tagged record
    R : Rec;
  end record;

end Opt90b_Pkg;
