-- { dg-compile }

package body Noreturn1 is

   procedure Error (E : in Exception_Occurrence) is
      Occurrence_Message : constant String := Exception_Message (E);
   begin
      if Occurrence_Message = "$" then
         raise Program_Error;
      else
         raise Constraint_Error;
      end if;
   end;

end Noreturn1;
