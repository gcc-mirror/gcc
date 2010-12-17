-- { dg-do compile }
-- { dg-options "-O -gnatn" }

package body Atomic4 is

   procedure Next (Self : in out Reader'Class) is
   begin
      Self.Current_Reference := Self.Reference_Stack.Last_Element;
      Self.Reference_Stack.Delete_Last;
   end Next;

end Atomic4;
