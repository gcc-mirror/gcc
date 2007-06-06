--  { dg-do run }

with GNAT.Expect; use GNAT.Expect;
with Ada.Text_IO; use Ada.Text_IO;
procedure expect1 is
   Process : Process_Descriptor;
begin
   begin
      Close (Process);
      raise Program_Error;
   exception
      when Invalid_Process =>
         null;  -- expected
   end;
end expect1;
