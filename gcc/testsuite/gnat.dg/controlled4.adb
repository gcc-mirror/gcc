-- { dg-do compile }
-- { dg-options "-gnatws" }

package body controlled4 is
   procedure Test_Suite is
   begin
      Add_Test
        (new Test_Case'(Test_Case1 with Link_Under_Test => 300));
   end Test_Suite;
end;
