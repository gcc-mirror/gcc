-- { dg-do compile }
-- { dg-options "-O" }

package body Discr41 is

   function F return Rec is
      Ret : Rec (0);
   begin
      return Ret;
   end;

end Discr41;
