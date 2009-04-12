package Enum1_Pkg is

  type Enum is (One, Two, Three);

  subtype Sub_Enum is Enum;

  My_N : Sub_Enum := One;

end Enum1_Pkg;
