-- { dg-do compile }

with Varsize3_Pkg1; use Varsize3_Pkg1;

procedure Varsize3_6 is

  Filter : Arr renames True.E;

begin
  null;
end;
