--  { dg-do run }
--  { dg-options "-gnatws" }

with System.OS_Lib; use System.OS_Lib;

procedure Split_Args is
   X : constant Argument_List_Access :=
     Argument_String_To_List (" -v");
begin
   if X'Length /= 1 then
      raise Program_Error;
   end if;
end Split_Args;
