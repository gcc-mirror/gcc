-- { dg-do compile }
-- { dg-options "-gnat2022" }

package body Aggr34_Pkg2 is
   procedure Disable_Prunt is
   begin
      My_Config.Set (["a", "b"]);
   end Disable_Prunt;
end Aggr34_Pkg2;
