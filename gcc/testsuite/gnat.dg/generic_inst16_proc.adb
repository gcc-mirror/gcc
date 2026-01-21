with Generic_Inst16_Pkg.Child.Grandchild;

procedure Generic_Inst16_Proc is
   package Inst_Grandchild is new Inst_Child.Grandchild;
begin
   null;
end;
