package Opt59_Pkg is

  type Boolean_Vector is array (1 .. 8) of Boolean;

  function Get_BV1 return Boolean_Vector;

  function Get_BV2 return Boolean_Vector;

  procedure Test (B : Boolean);

end Opt59_Pkg;
