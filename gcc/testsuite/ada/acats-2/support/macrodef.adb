with Ada.Text_IO;
with System;
procedure Macrodef is
begin
   Ada.Text_IO.Put_Line ("Integer'First = " & Integer'Image (Integer'First));
   Ada.Text_IO.Put_Line ("Integer'Last = " & Integer'Image (Integer'Last));
   Ada.Text_IO.Put_Line ("System.Min_Int = " & Long_Long_Integer'Image (System.Min_Int));
   Ada.Text_IO.Put_Line ("System.Max_Int = " & Long_Long_Integer'Image (System.Max_Int));
   Ada.Text_IO.Put_Line ("Ada.Text_IO.Count'Last = " & Ada.Text_IO.Count'Image (Ada.Text_IO.Count'Last));
   Ada.Text_IO.Put_Line ("Ada.Text_IO.Field'Last = " & Ada.Text_IO.Field'Image (Ada.Text_IO.Field'Last));
end Macrodef;
