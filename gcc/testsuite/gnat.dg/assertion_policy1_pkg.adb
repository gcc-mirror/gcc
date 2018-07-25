with Ada.Text_IO; use Ada.Text_IO;

package body Assertion_Policy1_Pkg is
   procedure Proc (Low : Integer; High : Integer) is
   begin
      Put_Line ("Proc");
   end Proc;
end Assertion_Policy1_Pkg;
