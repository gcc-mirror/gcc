-- { dg-do compile }
-- { dg-options "-O2" }

with Ada.Text_IO; use Ada.Text_IO;
with Assert;

package body Controlled_Record is
   
   procedure Assert_Invariants (PA : Point_T) is
     PB : Point_T;
   begin
      Assert.Assert (PB.Pos = PA.Pos);
   end;

end Controlled_Record;
