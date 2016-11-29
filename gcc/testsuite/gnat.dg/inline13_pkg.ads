package Inline13_Pkg is

  subtype Padded_T is String (1..8);

  type T is new Padded_T;

  function Padded (Value : T) return Padded_T;
  pragma Inline_Always (Padded);

end Inline13_Pkg;
