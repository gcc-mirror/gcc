-- { dg-do run }
-- { dg-options "-O2" }

with Ada.Command_Line; use Ada.Command_Line;

procedure Opt65 is

   procedure Check_Version_And_Help (Version_String : String) is
      Help_Switch_Present    : Boolean := False;
      Next_Arg               : Natural := 1;
   begin
      while Next_Arg <= Argument_Count loop
         declare
            Next_Argv : constant String := Argument (Next_Arg);
         begin
            if Next_Argv = "--help" then
               Help_Switch_Present := True;
            end if;
            Next_Arg := Next_Arg + 1;
         end;
      end loop;

      if Help_Switch_Present then
         raise Program_Error;
      end if;
   end;

begin
   Check_Version_And_Help ("version");
end;
