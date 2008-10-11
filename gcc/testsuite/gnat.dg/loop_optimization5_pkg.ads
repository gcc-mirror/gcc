package Loop_Optimization5_Pkg is

  type String_Access is access all String;
  function Init return String;
  function Locate (S : String) return String_Access;

end Loop_Optimization5_Pkg;
