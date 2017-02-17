package Array28_Pkg is

  subtype Outer_Type is String (1 .. 8);
  subtype Inner_Type is String (1 .. 5);

  function F return Inner_Type;

end Array28_Pkg;
