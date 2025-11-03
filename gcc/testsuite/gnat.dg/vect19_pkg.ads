package Vect19_Pkg is

  type Arr is array (1 .. 4) of Float;
  for Arr'Alignment use 16;

  function Sum (X : Arr; Y : Arr) return Arr;
  pragma Inline (Sum);

end Vect19_Pkg;
