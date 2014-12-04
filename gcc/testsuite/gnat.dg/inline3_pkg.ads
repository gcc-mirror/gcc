package Inline3_Pkg is

  procedure Test (I : Integer);
  pragma Inline_Always (Test);

end Inline3_Pkg;
