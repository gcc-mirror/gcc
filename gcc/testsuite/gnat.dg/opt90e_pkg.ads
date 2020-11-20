package Opt90e_Pkg is

  type Rec is record
    D : String (1 .. 12);
    A : Short_Short_Integer;
    B : Integer;
    C : Short_Integer;
  end record;
  pragma Pack (Rec);
  for Rec'Alignment use 1;

  type Data is tagged record
    R : Rec;
  end record;

end Opt90e_Pkg;
