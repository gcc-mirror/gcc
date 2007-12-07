-- { dg-do run }
with Ada.Directories;
with Ada.Text_IO;

procedure Compose is
   Result : constant String := Ada.Directories.Compose (Name      => "foo",
                                                        Extension => "txt");
   pragma Unreferenced (Result);
begin
   null;
end Compose;
