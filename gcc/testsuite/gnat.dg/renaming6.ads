package Renaming6 is

  I : Integer;
  pragma Atomic (I);

  function Get_I return Integer;
  procedure Set_I (Val : Integer);

  J : Integer renames I;

  function Get_J return Integer;
  procedure Set_J (Val : Integer);

end Renaming6;
