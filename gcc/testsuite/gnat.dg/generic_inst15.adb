--  { dg-do compile }

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with Generic_Inst15_Pkg;
with Generic_Inst15_Pkg.G;

procedure Generic_Inst15 is

  procedure Print_Word
     (Word     : in out Generic_Inst15_Pkg.Word_Type;
      Continue :    out Boolean)
  is
  begin
     Ada.Text_IO.Put_Line(Generic_Inst15_Pkg.Get_Word(Word));
     Continue := True;
  end;

  package Word_Lister is new Generic_Inst15_Pkg.G
     (Order   => Generic_Inst15_Pkg.Word_Order'Val (Positive'Value (Argument(1))),
      Process => Print_Word);

begin
   null;
end;
