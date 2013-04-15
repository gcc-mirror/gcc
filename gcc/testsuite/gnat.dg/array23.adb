-- { dg-do link }

with Array23_Pkg1;
with Array23_Pkg2;

procedure Array23 is
  A : Array23_Pkg1.Arr;
begin
  A(Array23_Pkg2.One)(1) := 0;
end;
