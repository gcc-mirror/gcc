package body Discr25_Pkg is

  type Arr1 is array (Natural range <>) of Integer;

  B : constant Boolean := N > 0;

  type Arr2 is array (True .. B) of Integer;

  type Obj_T (Size_Max : Natural) is record
    A2 : Arr2;
    A1 : Arr1 (0 .. Size_Max);
  end record;

  procedure Proc1 (Set : in out T) is
  begin
    Set := new Obj_T'(Set.all);
  end;

  procedure Proc2 (Obj : in out T; L : Natural) is
  begin
    Obj := new Obj_T (L);
  end;

end Discr25_Pkg;
