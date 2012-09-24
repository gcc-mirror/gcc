-- { dg-do compile }
-- { dg-options "-w" }

package body Vect8 is

   function Foo (V : Vec) return Vec is
      Ret : Vec;
   begin
      Ret (1) := V (1) + V (2);
      Ret (2) := V (1) - V (2);
      return Ret;
   end;

end Vect8;
