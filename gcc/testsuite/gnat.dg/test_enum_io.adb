--  { dg-do run }

with Ada.Text_IO;
use  Ada.Text_IO;

procedure Test_Enum_IO is

  type Enum is (Literal);
  package Enum_IO is new Enumeration_IO (Enum);
  use Enum_IO;

  File : File_Type;
  Value: Enum;
  Rest : String (1 ..30);
  Last : Natural; 
  
begin
  
  Create (File, Mode => Out_File);
  Put_Line (File, "Literax0000000l note the 'l' at the end");
  
  Reset (File, Mode => In_File);
  Get (File, Value); 
  Get_Line (File, Rest, Last);
  
  Close (File);
  
  Put_Line (Enum'Image (Value) & Rest (1 .. Last));
  raise Program_Error;

exception
  when Data_Error => null;
end Test_Enum_IO;
