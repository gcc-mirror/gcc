package body Thunk1_Pkg2 is

  procedure Op (This : in out Ext; S : String) is
  begin
    if S /= "Message" then
      raise Program_Error;
    end if;
  end;

end Thunk1_Pkg2;
