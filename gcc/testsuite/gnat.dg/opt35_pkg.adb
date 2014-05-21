package body Opt35_Pkg is

  function F (I : Integer) return Integer is
  begin
    if I = 0 then
       raise E;
    end if;
    return -I;
  end;

end Opt35_Pkg;
