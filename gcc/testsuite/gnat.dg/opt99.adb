-- { dg-do run }
-- { dg-options "-O" }

with Opt99_Pkg1; use Opt99_Pkg1;

procedure Opt99 is
  C : constant My_Character := (D => True, C => ' ');
  D : Derived;

begin
  Set (D, C, C);
  if not D.C2.D then
    raise Program_Error;
  end if;
end;
