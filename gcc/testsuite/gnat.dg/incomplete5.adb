-- { dg-do compile }

package body Incomplete5 is

   function Get (O: Base_Object) return Integer is
   begin
      return Get_Handle(O).I;
   end;

end Incomplete5;
