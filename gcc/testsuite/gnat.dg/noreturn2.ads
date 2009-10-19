with Ada.Exceptions; use Ada.Exceptions;

package Noreturn2 is

   procedure Raise_From (X : Exception_Occurrence);
   pragma No_Return (Raise_From);

end Noreturn2;
