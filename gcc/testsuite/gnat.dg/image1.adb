--  { dg-do run }

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;

procedure Image1 is
   Str : String := Ada.Characters.Latin_1.LF'Img;
begin
   if Str /= "LF" then
      raise Program_Error;
   end if;
end Image1;
