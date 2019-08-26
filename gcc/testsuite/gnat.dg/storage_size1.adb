--  { dg-do compile }

with Ada.Text_IO; with Ada.Integer_Text_IO;

procedure Storage_Size1 is

  package O renames Ada.Text_IO;
  package T renames Ada.Integer_Text_IO;

  type Struct is record first, second: Integer; end record;

  type SP is access Struct
      with Storage_Size => 64 * Struct'Max_Size_In_Storage_Elements;

begin

  T.Put(SP'Storage_Size); O.New_Line(1);

end Storage_Size1;
