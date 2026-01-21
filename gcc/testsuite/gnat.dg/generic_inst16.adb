-- { dg-do link }

with Generic_Inst16_Pkg.Child.Grandchild;
with Generic_Inst16_Proc;

procedure Generic_Inst16 is
   package   P1 is new Generic_Inst16_Pkg.Child;
   procedure P2 is new Generic_Inst16_Proc (P1);
begin
   null;
end;
