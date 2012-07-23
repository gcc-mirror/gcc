-- { dg-do compile }

package body Aggr20 is

   procedure Proc (R : out Rec3) is
   begin
      R := (Callback => Nil_Rec2);
   end;

end Aggr20;
