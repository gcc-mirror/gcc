-- { dg-do compile }

package body Weak2 is

   function F return Integer is
   begin
      return Var;
   end;

end Weak2;
