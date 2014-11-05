package Inline4_Pkg is

  procedure Test (I : Integer);
  pragma Inline_Always (Test);

end Inline4_Pkg;
