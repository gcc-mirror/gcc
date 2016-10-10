package body Inline13_Pkg is

  function Padded (Value : T) return Padded_T is
  begin
    return Padded_T(Value);
  end Padded;

end Inline13_Pkg;
