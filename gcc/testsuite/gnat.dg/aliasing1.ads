package Aliasing1 is

  type Rec is record
    I : Integer;
  end record;

  type Ptr is access all Integer;

  R : Rec;

  function F (P : Ptr) return Integer;

end Aliasing1;
