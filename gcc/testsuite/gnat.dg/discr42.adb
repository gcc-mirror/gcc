-- { dg-do run }

with Discr42_Pkg; use Discr42_Pkg;

procedure Discr42 is

  R : Rec;
  Pos : Natural := 1;

begin

  R := F (Pos);

  if Pos /= 2 then
    raise Program_Error;
  end if;

  if R /= (D => True, N => 4) then
    raise Program_Error;
  end if;

end;
