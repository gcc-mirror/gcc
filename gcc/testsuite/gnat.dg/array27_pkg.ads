package Array27_Pkg is

  subtype Outer_Type is String (1 .. 8);
  subtype Inner_Type is String (1 .. 3);

  function F return Inner_Type;

end Array27_Pkg;
