--  { dg-do run }

with Tagged_Type_Pkg;  use Tagged_Type_Pkg;
with Ada.Text_IO;      use Ada.Text_IO;
      
procedure Aliased_Prefix_Accessibility is
   
  T_Obj : aliased TT;
         
   T_Obj_Acc : access TT'Class := T_Obj'Access;
   
   type Nested_TT is limited record
      TT_Comp : aliased TT;
   end record;

   NTT_Obj : Nested_TT;

   ATT_Obj : array (1 .. 2) of aliased TT;

begin
   begin
      T_Obj_Acc := Pass_TT_Access (T_Obj'Access);
      Put_Line ("FAILED (1): call should have raised an exception");
   exception
      when others =>
         null;
   end;

   begin
      T_Obj_Acc := T_Obj.Pass_TT_Access;
      Put_Line ("FAILED (2): call should have raised an exception");
   exception
      when others =>
         null;
   end;

   begin
      T_Obj_Acc := Pass_TT_Access (NTT_Obj.TT_Comp'Access);
      Put_Line ("FAILED (3): call should have raised an exception");
   exception
      when others =>
         null;
   end;
   
   begin
      T_Obj_Acc := NTT_Obj.TT_Comp.Pass_TT_Access;
      Put_Line ("FAILED (4): call should have raised an exception");
   exception
      when others =>
         null;
   end;
   
   begin
      T_Obj_Acc := Pass_TT_Access (ATT_Obj (1)'Access);
      Put_Line ("FAILED (5): call should have raised an exception");
   exception
      when others =>
         null;
   end;
   
   begin
      T_Obj_Acc := ATT_Obj (2).Pass_TT_Access;
      Put_Line ("FAILED (6): call should have raised an exception");
   exception
      when others =>
         null;
   end;
end Aliased_Prefix_Accessibility;
