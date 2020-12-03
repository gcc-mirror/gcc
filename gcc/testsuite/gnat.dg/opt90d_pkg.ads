package Opt90d_Pkg is

  type Rec is record
    D : String (1 .. 12);
    C : Short_Integer;
    A : Short_Short_Integer;
    B : Integer;
  end record;
  pragma Pack (Rec);
  for Rec'Alignment use 1;

  type Data is tagged record
    R : Rec;
  end record;

end Opt90d_Pkg;
