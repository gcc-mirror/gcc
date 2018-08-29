--  { dg-do run }
--  { dg-options "-gnata" }

with Ada.Text_IO; use Ada.Text_IO;
with Assertion_Policy1_Pkg; use Assertion_Policy1_Pkg;

procedure Assertion_Policy1 is
begin
   Proc (2, 1);

exception
   when others =>
      Put_Line ("ERROR: unexpected exception");
      raise;
end Assertion_Policy1;
