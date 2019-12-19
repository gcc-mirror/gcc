--  { dg-do compile }
--  { dg-options "-O -gnatn -Winline -cargs --param max-inline-insns-single=50 -margs" }

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

procedure Warn32 is
  type Selected_Block_T is record
    Contents  : Unbounded_String;
    File_Name : Unbounded_String;
  end record;

  pragma Warnings (Off, "-Winline");
  package Selected_Block_List is
    new Ada.Containers.Vectors (Natural, Selected_Block_T);
begin
  Ada.Text_Io.Put_Line ("Hello World!");
end;
