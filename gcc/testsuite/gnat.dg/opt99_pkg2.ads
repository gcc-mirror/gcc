package Opt99_Pkg2 is

  function Get_Max return Positive is (4);

  C : constant Positive := Get_Max;

  type Arr is array (1 .. C) of Integer;

  type Root is tagged record
    Data : Arr;
  end record;

end Opt99_Pkg2;
