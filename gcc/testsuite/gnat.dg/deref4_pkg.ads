package Deref4_Pkg is

  type A is tagged null record;
  type A_Ptr is access A;
  procedure Proc (This : in out A'Class; Some_Parameter : A_Ptr) is null;
  Obj : A_Ptr;

end Deref4_Pkg;
