-- { dg-do compile }
-- { dg-options "-O" }

with Sizetype3_Pkg; use Sizetype3_Pkg;

package body Sizetype3 is

   procedure Handle_Enum_Values is
      Values : constant List := F;
      L : Values_Array_Access;
   begin
      L := new Values_Array (1 .. Values'Length);
   end Handle_Enum_Values;

   procedure Simplify_Type_Of is
   begin
      Handle_Enum_Values;
   end Simplify_Type_Of;

end Sizetype3;
