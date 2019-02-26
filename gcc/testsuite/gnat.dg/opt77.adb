-- { dg-do run }
-- { dg-options "-O -fno-inline" }

with Opt77_Pkg; use Opt77_Pkg;

procedure Opt77 is
  N : Natural := 0;
  To_Add : Boolean;
begin
  Proc ("One", N, To_Add);
  if To_Add then
    raise Program_Error;
  end if;
end;
