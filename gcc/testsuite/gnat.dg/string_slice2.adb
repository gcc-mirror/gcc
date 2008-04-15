-- { dg-do compile }
-- { dg-options "-O" }

with Ada.Strings;
with Ada.Strings.Fixed;

procedure String_Slice2 is

   package ASF renames Ada.Strings.Fixed;

   Delete_String  : String(1..10);
   Source_String2 : String(1..12) := "abcdefghijkl";

begin

   Delete_String := Source_String2(1..10);

   ASF.Delete(Source  => Delete_String,
              From    => 6,
              Through => Delete_String'Last,
              Justify => Ada.Strings.Left,
              Pad     => 'x');

end;
