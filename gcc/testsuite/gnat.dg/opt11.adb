-- { dg-do compile }
-- { dg-options "-O" }

package body Opt11 is

   procedure Proc is
      R : Rec;
   begin
      R := (others => <>);  --  { dg-warning "aggregate not fully initialized" }
   end;

end Opt11;
