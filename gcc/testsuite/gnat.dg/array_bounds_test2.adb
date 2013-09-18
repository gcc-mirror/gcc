--  { dg-do run }

with Ada.Unchecked_Deallocation;

procedure Array_Bounds_Test2 is

  type String_Ptr_T is access String;
  procedure Free is new Ada.Unchecked_Deallocation (String, String_Ptr_T);
  String_Data : String_Ptr_T := new String'("Hello World");

  function Peek return String_Ptr_T is
  begin
    return String_Data;
  end Peek;

begin
  declare
    Corrupted_String : String := Peek.all;
  begin
    Free(String_Data);
    if Corrupted_String'First /= 1 then
      raise Program_Error;
    end if;
  end;
end;
