package body Opt94_Pkg is

  function Worker (S : String) return Integer;
  pragma Pure_Function (Worker);

  function Valid_Result (S : String) return Boolean is
  begin
    return Worker (S) > 0;
  end;

  function Result (S : String) return Integer is
    R : constant Integer := Worker (S);
  begin
    if R > 0 then
      return R;
    else
      raise Program_Error;
    end if;
  end;

  function Worker (S : String) return Integer is
  begin
    return Character'Pos (S (S'First));
  end;

  function Get return String is
  begin
    return "";
  end;

end Opt94_Pkg;
