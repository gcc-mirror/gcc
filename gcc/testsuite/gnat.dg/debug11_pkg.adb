--  { dg-options "-cargs -g -dA -margs" }
--  { dg-final { scan-assembler "local_imported_func" } }
--  { dg-final { scan-assembler "local_imported_var" } }
--  { dg-final { scan-assembler "global_imported_func" } }
--  { dg-final { scan-assembler "global_imported_var" } }
--  { dg-final { scan-assembler-not "foreign_imported_func" } }
--  { dg-final { scan-assembler-not "foreign_imported_var" } }

with Debug11_Pkg2;

package body Debug11_Pkg is

   procedure Dummy is
      Local_Imported_Var : Integer;
      pragma Import (C, Local_Imported_Var, "imported_var");

      function Local_Imported_Func return Integer;
      pragma Import (C, Local_Imported_Func, "imported_func");
   begin
      Local_Imported_Var := Local_Imported_Func;
      Global_Imported_Var := Global_Imported_Func;
      Debug11_Pkg2.Foreign_Imported_Var :=
         Debug11_Pkg2.Foreign_Imported_Func;
   end Dummy;

end Debug11_Pkg;
