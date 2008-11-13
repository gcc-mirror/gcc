--  { dg-do run }
--  { dg-options "-O2" }
with Ada.Text_Io; use Ada.Text_IO;
with Raise_From_Pure; use Raise_From_Pure;
procedure handle_raise_from_pure is
   K : Integer;
begin
   K := Raise_CE_If_0 (0);
exception
   when others => Put_Line ("exception caught");
end;
