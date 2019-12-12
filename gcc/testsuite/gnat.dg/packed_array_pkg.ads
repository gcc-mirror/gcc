package Packed_Array_Pkg is

  type Rec1 is record
    I : Integer;
    S : Short_Integer;
  end record;
  for Rec1'Size use 49;

  type Arr is array (Positive range <>) of Rec1;
  for Arr'Component_Size use 49;

  type Rec2 (Lo, Hi : Positive) is record
    A : Arr (Lo .. Hi);
  end record;

  Max : Positive := 1;

  subtype Small_Rec2 is Rec2 (1, Max);

end;
