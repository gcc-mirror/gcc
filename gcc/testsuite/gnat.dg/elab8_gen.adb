with Elab8_Pkg;

package body Elab8_Gen is

  procedure Compare (Arg1, Arg2 : T) is
  begin
    if Arg1 = Arg2 then
      raise Program_Error;
    end if;
  end;

end Elab8_Gen;
