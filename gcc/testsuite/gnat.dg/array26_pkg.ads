package Array26_Pkg is

  subtype Outer_Type is String (1 .. 4);
  subtype Inner_Type is String (1 .. 3);

  function F return Inner_Type;

end Array26_Pkg;
