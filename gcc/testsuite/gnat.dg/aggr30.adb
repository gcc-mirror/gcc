-- { dg-do compile }
-- { dg-options "-fdump-tree-gimple" }

package body Aggr30 is

   Null_Constant : constant Rec := (Data => (others => 0),
                                    Padding => (others => 0));
   procedure Init is
   begin
      Instance := Null_Constant;
   end;

   procedure Init_Volatile is
   begin
      Instance_Volatile := Null_Constant;
   end;

end Aggr30;

-- { dg-final { scan-tree-dump-times "= {}" 2 "gimple"} }
