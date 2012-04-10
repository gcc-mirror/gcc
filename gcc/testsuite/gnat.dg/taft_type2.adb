-- { dg-do compile }
-- { dg-options "-g" }

with Taft_Type2_Pkg; use Taft_Type2_Pkg;

package body Taft_Type2 is

   procedure Proc is
      A : T;

      function F return T is
         My_T : T;
      begin
         My_T := Open;
         return My_T;
      end;

   begin
      A := F;
   end;

end Taft_Type2;
