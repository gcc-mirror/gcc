package body Opt59_Pkg is

  function Get_BV1 return Boolean_Vector is
  begin
    return (others => True);
  end;

  function Get_BV2 return Boolean_Vector is
  begin
    return (others => False);
  end;

  procedure Test (B : Boolean) is
  begin
    if not B then
      raise Program_Error;
    end if;
  end;

end Opt59_Pkg;
