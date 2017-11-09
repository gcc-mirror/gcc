--  { dg-do compile }

pragma Warnings
  (Off, "anonymous access-to-controlled object will be finalized when its enclosing unit goes out of scope");

with controlled1; use controlled1;
package body controlled2 is
   procedure Test_Suite is
   begin
      Add_Test
        (new Test_Case'(Test_Case1 with Link_Under_Test => 300));
   end Test_Suite;
end controlled2;
