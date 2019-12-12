--  { dg-do run }

with Ada.Text_IO; use Ada.Text_IO;
with System;      use System;

procedure Equal9 is
   Val : Address := Null_Address;
begin
   if Val = Null_Address then
      Put_Line ("= OK");
   else
      raise Program_Error;
   end if;

   if Val /= Null_Address then
      raise Program_Error;
   else
      Put_Line ("/= OK");
   end if;

   if not (Val = Null_Address) then
      raise Program_Error;
   else
      Put_Line ("not = OK");
   end if;
end Equal9;
