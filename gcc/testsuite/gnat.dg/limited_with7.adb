-- { dg-do compile }

with Limited_With7_Pkg; use Limited_With7_Pkg;

package body Limited_With7 is

   procedure Proc (R : out Limited_With7_Pkg.Rec) is
   begin
      R.I := 0;
   end;

end Limited_With7;
