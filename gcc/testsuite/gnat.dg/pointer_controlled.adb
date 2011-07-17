-- PR ada/49732
-- Testcase by Vorfeed Canal

-- { dg-do compile }
-- { dg-options "-gnato" }

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces.C.Pointers;

procedure Pointer_Controlled is

   function Create (Name : String) return size_t is

      type Name_String is new char_array (0 .. Name'Length);
      type Name_String_Ptr is access Name_String;
      pragma Controlled (Name_String_Ptr);

      Name_Str : constant Name_String_Ptr := new Name_String;
      Name_Len : size_t;

   begin
      To_C (Name, Name_Str.all, Name_Len);
      return 1;
   end;

   Test : size_t;

begin
   Test := Create("ABC");
end;
