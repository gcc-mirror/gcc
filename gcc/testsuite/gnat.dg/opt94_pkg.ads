package Opt94_Pkg is

  function Valid_Result (S : String) return Boolean;
  pragma Inline (Valid_Result);

  function Result (S : String) return Integer;
  pragma Inline (Result);

  function Get return String;

end Opt94_Pkg;
