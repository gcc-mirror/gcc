--  { dg-do run }

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Fixed_Delete is
   Str  : String := "a";
   Str1 : String := Replace_Slice (Str, 2, 2, "");
   Str2 : String := Delete (Str, 2, 2);
begin
   if Str1 /= "a" then
      raise Program_Error;
   end if;
   if Str2 /= "a" then
      raise Program_Error;
   end if;
end Fixed_Delete;
