with Ada.Exceptions; use Ada.Exceptions;

package Noreturn1 is

   procedure Error (E : in Exception_Occurrence);
   pragma No_Return (Error);

end Noreturn1;
