package body Loop_Optimization8_Pkg2 is

  function Length (Set : T) return Natural is
  begin
    return Set.Length;
  end Length;

  function Index (Set : T; Position : Natural) return Integer is
  begin
    return Set.Elements (Position);
  end Index;

end Loop_Optimization8_Pkg2;
