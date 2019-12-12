--  { dg-do run }
--  { dg-options "-O2" }

with Ada.Text_IO; use Ada.Text_IO;

procedure Opt80 is
   Item : Integer;
begin
   Item := Integer'Value ("zzz");
   Put_Line (Boolean'Image (Item'Valid));
   raise Program_Error;
exception
   when Constraint_Error =>
      null;
end;
