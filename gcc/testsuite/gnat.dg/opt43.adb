-- { dg-do compile }
-- { dg-options "-O -gnatws" }

procedure Opt43 is

  function Func return Integer is
  begin
    if False then
       return 0;
    end if;
  end;

begin

  if Func = Func then
    raise Program_Error;
  end if;

exception
  when Program_Error => null;
end;
