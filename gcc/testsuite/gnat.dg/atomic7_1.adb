-- { dg-do run }

with Atomic7_Pkg2; use Atomic7_Pkg2;

procedure Atomic7_1 is

  I : Integer := Stamp;
  pragma Atomic (I);

  J : Integer := Stamp;

begin
  if I /= 1 then
    raise Program_Error;
  end if;
end;
