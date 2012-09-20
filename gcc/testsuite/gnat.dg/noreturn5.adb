-- { dg-do compile }

with Ada.Characters.Handling; use Ada.Characters.Handling;
with GNAT.OS_Lib;             use GNAT.OS_Lib;
with Text_IO;                 use Text_IO;

package body Noreturn5 is

   procedure Proc (Arg_Line : Wide_String; Keep_Going : Boolean) is
   begin
      Put (To_String (Arg_Line));

      if Keep_Going then
         raise Constraint_Error;
      else
         OS_Exit (1);
      end if;

   exception
      when Constraint_Error =>
         raise;

      when others =>
         if Keep_Going then
            raise Constraint_Error;
         else
            OS_Exit (1);
         end if;

   end;

end Noreturn5;
