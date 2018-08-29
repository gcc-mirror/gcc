--  { dg-do compile }
--  { dg-options "-cargs -gdwarf-4 -fdebug-types-section -dA -margs" }
--  { dg-final { scan-assembler-times "DW_AT_location" 4 } }

package body Debug12 is
   function Get_A2 return Boolean is
   begin
      return A2;
   end Get_A2;
end Debug12;
