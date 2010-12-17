-- { dg-compile }
-- { dg-options "-O" }

package body Opt11 is

   procedure Proc is
      R : Rec;
   begin
      R := (others => <>);
   end;

end Opt11;
