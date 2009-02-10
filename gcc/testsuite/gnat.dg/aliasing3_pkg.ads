package Aliasing3_Pkg is

  type Arr is array (1..3) of Integer;

  procedure Test (A : Arr);
  pragma Inline (Test);

  type My_Arr is new Arr;

  type Rec is record
    A : My_Arr;
  end record;

  type Ptr is access all Rec;

  Block : aliased Rec;
  Pointer : Ptr := Block'Access;

end Aliasing3_Pkg;
