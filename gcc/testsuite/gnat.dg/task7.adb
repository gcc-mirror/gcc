-- { dg-do compile }

with Task7_Pkg; use Task7_Pkg;

procedure Task7 is
  A : Ptr := new Ext'(Task_Wrapper with I => 1);
begin
  null;
end;
