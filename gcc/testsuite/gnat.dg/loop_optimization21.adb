-- { dg-do compile }
-- { dg-options "-O -fdump-tree-optimized" }

package body Loop_Optimization21 is

  function Min (X : in Item_Vector) return Item is
    Tmp_Min : Item;
  begin
    Tmp_Min := X (X'First);
    for I in X'First + 1 .. X'Last loop
      if X (I) <= Tmp_Min then
        Tmp_Min := X (I);
      end if;
    end loop;
    return Tmp_Min;
  end Min;

end Loop_Optimization21;

-- { dg-final { scan-tree-dump-times "Index_Check" 1 "optimized" } }
