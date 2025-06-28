-- { dg-do compile }

with Ada.Text_IO; use Ada.Text_IO;

procedure Concat6 is
  C : constant character := 16#00#; -- { dg-error "expected type|found type" }
begin
  Put_Line ("Test " & C);
end;
