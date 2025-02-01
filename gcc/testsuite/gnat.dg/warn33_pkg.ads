package Warn33_Pkg is

  type GenT is delta 1.0 range 1.0 .. 10.0;
  function "-" (X : GenT; Y : GenT) return GenT;
  type DerT is new GenT;

end Warn33_Pkg;
