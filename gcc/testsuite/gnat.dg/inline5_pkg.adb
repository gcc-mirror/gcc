package body Inline5_Pkg is

  procedure Test (I : Integer) is

    function F (J : Integer) return Integer is
    begin
      return I - J;
    end;

  begin
    if I /= F (I) then
      raise Program_Error;
    end if;
  end;

end Inline5_Pkg;
