-- { dg-do compile }

with Limited_With2_Pkg2;

package body Limited_With2 is

   function Func (Val : Rec1) return Limited_With2_Pkg1.Rec2 is
   begin
      return Val.F;
   end;

end Limited_With2;
