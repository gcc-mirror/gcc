package body Noinline3_Pkg is

  function Inner (A, B : Integer) return Integer;
  pragma No_Inline (Inner);

  function Inner (A, B : Integer) return Integer is
  begin
    return A + B;
  end;

  function F (A, B : Integer) return Integer is
  begin
    return Inner (A, B) + Inner (A, -B);
  end;

end Noinline3_Pkg;
