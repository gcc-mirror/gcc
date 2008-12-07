-- { dg-do compile }

with Ada.Text_IO; use Ada.Text_IO;

procedure String_Comparison is
   package Bool_IO is new Enumeration_IO (Boolean);
   use Bool_IO;
begin
   Put (Boolean'Image (True) = "True");
end;
