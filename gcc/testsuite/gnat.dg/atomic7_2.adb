--- { dg-do run }

with Atomic7_Pkg1; use Atomic7_Pkg1;

procedure Atomic7_2 is
begin
  if I /= 1 then
    raise Program_Error;
  end if;
end;
