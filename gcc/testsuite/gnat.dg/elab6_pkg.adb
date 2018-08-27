package body Elab6_Pkg is
   protected Prot_Obj is
      entry Ent;
   end Prot_Obj;

   procedure Call_Ent is
   begin
      Prot_Obj.Ent;
   end Call_Ent;

   protected body Prot_Obj is
      entry Ent when True is
      begin
         null;
      end Ent;
   end Prot_Obj;
end Elab6_Pkg;
