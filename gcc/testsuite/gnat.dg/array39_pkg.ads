package Array39_Pkg is

  subtype Index1 is Natural range 0 .. 2;

  type Arr1 is array (Index1 range <>) of Integer;

  type Rec1 (D : Index1 := 0) is record
    A : Arr1 (1 .. D);
  end record;

  subtype Index2 is Natural range 0 .. 7;

  type Arr2 is array (Index2 range <>) of Rec1;

  type Rec2 (D : Index2 := 0) is record
    A : Arr2 (1 .. D);
  end record;

  Val : Rec1 := (D => 1, A => (others => 1));

  task type Tsk is
    entry E (R : out Rec2; L : Index2);
  end Tsk;

end Array39_Pkg;
