package Prefix4_Pkg is

  type T (D : Integer) is private;

  function F (X : T) return Boolean is (True);

private

  type T (D : Integer) is record
    F : Boolean := False;
  end record;

end Prefix4_Pkg;
