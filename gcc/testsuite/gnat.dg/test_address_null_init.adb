--  { dg-do run }
--  { dg-options "-gnatws" }

with Address_Null_Init;  use Address_Null_Init;
with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Address_Null_Init is
begin
   if B /= null then
      Put_Line ("ERROR: B was not default initialized to null!");
   end if;
   
   if A /= null then
      Put_Line ("ERROR: A was not reinitialized to null!");
   end if;
end Test_Address_Null_Init;
