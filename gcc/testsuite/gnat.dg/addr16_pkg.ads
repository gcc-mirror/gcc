package Addr16_Pkg is

  type Arr is array (Positive range <>) of Long_Long_Integer;

  type Rec (D : Positive) is record
    A : Arr (1 .. D);
  end record;

end Addr16_Pkg;
