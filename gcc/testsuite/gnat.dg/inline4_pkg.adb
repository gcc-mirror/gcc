package body Inline4_Pkg is

  procedure Test (I : Integer) is

    function F (J : Integer) return Integer is
    begin
      return I - J;
    end;
    pragma Inline_Always (F);

  begin
    if I /= F (I) then
      raise Program_Error;
    end if;
  end;

end Inline4_Pkg;
