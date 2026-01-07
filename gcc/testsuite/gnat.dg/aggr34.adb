-- PR ada/123302
-- { dg-do link }
-- { dg-options "-gnat2022" }

with Aggr34_Pkg3;
with Aggr34_Pkg1;

procedure Aggr34 is

  package My_Pkg3 is new Aggr34_Pkg3;
  package My_Pkg1 is new Aggr34_Pkg1 (My_Pkg3);

begin
  My_Pkg1.Proc;
end;
