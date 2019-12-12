--  { dg-do compile }
--  { dg-options "-Wuninitialized" }

with Ada.Text_IO; use Ada.Text_IO;

procedure Warn18 is
   type Set is array (Natural range <>) of Boolean;
   pragma Pack (Set);

   O : constant Set (0 .. 255) := (28 => True, others => False);
begin
   Put_Line (O (1)'Img);
end Warn18;
