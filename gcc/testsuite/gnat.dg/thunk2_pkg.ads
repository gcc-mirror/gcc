package Thunk2_Pkg is

  type Root is tagged record
    A : Integer;
  end record;

  type I is interface;

  function Element (Self : I; Name : String) return I is abstract;

end Thunk2_Pkg;
