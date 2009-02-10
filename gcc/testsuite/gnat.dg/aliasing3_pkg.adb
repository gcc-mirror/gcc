package body Aliasing3_Pkg is

  procedure Test (A : Arr) is
  begin
    if A(1) /= 5 then
      raise Program_Error;
    end if;
  end;

end Aliasing3_Pkg;
