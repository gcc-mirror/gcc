package body Inline10_Pkg is

  procedure Test (I : Integer) is

    function F (J : Integer) return Integer is
    begin
      return I - J;
    end;
    pragma Inline (F);

    type FPT is access function (I : Integer) return Integer;

    P : FPT := F'Access;

  begin
    if I /= P (I) then
      raise Program_Error;
    end if;
  end;

end Inline10_Pkg;
