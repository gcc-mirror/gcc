-- { dg-do run }
-- { dg-options "-gnat2022" }

with Ada.Text_IO; use Ada.Text_IO;

procedure Put_Image1 is

  type Client_ID_Part is range 0 .. 2**32 - 1 with Size => 32;

  type Client_ID is array (1 .. 2) of Client_ID_Part;

  A : Client_ID := (1479222903, 3163714999);

begin
   Put_Line (A'Image);
   -- { dg-output ".* 1479222903,  3163714999.*\n" }
   Put_Line (A (1)'Image);
   -- { dg-output " 1479222903.*\n" }
   Put_Line (A (2)'Image);
   -- { dg-output " 3163714999.*\n" }
end;
