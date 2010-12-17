-- { dg-do compile }

with Discr25_Pkg;

procedure Discr25 (N : Natural) is

  package Test_Set is new Discr25_Pkg (N);

begin
  null;
end;
