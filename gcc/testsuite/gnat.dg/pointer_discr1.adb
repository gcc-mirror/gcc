-- { dg-do compile }

with Pointer_Discr1_Pkg1;
with Pointer_Discr1_Pkg3;

procedure Pointer_Discr1 is
begin
  Pointer_Discr1_Pkg3.Map(Pointer_Discr1_Pkg1.Window(1));
end;
