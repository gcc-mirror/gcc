-- { dg-do run }
-- { dg-options "-O" }

with Opt35_Pkg; use Opt35_Pkg;

procedure Opt35 is
  I : Integer := -1;
  N : Natural := 0;
begin
  begin
    I := F(0);
  exception
    when E => N := N + 1;
  end;

  begin
    I := I + F(0);
  exception
    when E => N := N + 1;
  end;

  if N /= 2 or I = 0 then
    raise Program_Error;
  end if;
end;
