-- { dg-do compile }

package body Noreturn2 is

   procedure Raise_Exception_No_Defer (Message : String);
   pragma No_Return (Raise_Exception_No_Defer);

   procedure Raise_From (X : Exception_Occurrence) is
      Occurrence_Message : constant String := Exception_Message (X);
   begin
      if Occurrence_Message = "$" then
         Raise_Exception_No_Defer (Occurrence_Message);
      else
         Raise_Exception_No_Defer ("::" & Occurrence_Message);
      end if;
   end;

   procedure Raise_Exception_No_Defer (Message : String) is
   begin
     raise Program_Error;
   end;

end Noreturn2;
