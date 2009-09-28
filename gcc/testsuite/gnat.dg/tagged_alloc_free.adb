-- { dg-do run }

with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Tagged_Alloc_Free is

  type Test_Base is tagged null record;
  type Test_Class_Access is access all Test_Base'Class;
  type Test_Extension is new Test_Base with record
    Last_Name : Unbounded_String := Null_Unbounded_String;
  end record;

  procedure Free is new Ada.Unchecked_Deallocation
    (Object => Test_Base'Class,
     Name   => Test_Class_Access);

  Handle : Test_Class_Access := new Test_Extension;

begin
  Free (Handle);
end;
