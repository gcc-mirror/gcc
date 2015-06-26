-- { dg-do compile }

with Ada.Text_IO; use Ada.Text_IO;

procedure Warn11 is

   type My_Integer is new Integer range 1 .. 10;
   for My_Integer'Size use 65;  -- { dg-warning "unused" }

   type My_Integer2 is new Integer range 1 .. 10;
   for My_Integer2'Size use 129;  -- { dg-warning "unused" }

begin
   Put_Line ("MB'Size is " & Natural'Image (My_Integer'Size));
   Put_Line ("MB'Size is " & Natural'Image (My_Integer2'Size));
end;
