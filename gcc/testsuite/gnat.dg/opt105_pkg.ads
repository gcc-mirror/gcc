package Opt105_Pkg is

  type Enum is (One, Two, Three);

  Enabled  : Boolean := False;
  Disabled : Boolean := False;

  function Cond1 return Boolean;
  function Cond2 return Boolean;

end Opt105_Pkg;
