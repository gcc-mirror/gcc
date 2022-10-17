package body Concat5_Pkg2 is

  procedure Compare (S : String) is
  begin
    if S /= "option -RTS=none should start with '--'" then
      raise Program_Error;
    end if;
  end;

end Concat5_Pkg2;
