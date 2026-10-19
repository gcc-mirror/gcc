-- { dg-do compile }
-- { dg-options "-gnatwu" }

with Ada.Strings.Fixed;
with Ada.Text_IO;

procedure Renaming20 is

  use Ada.Strings;
  use Fixed;

  function T (Item : String; Side : Trim_End) return String
    renames Trim;

begin
  Ada.Text_IO.Put_Line (T (" Hello ", Left));
end;
