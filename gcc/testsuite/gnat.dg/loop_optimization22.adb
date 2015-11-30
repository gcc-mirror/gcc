-- { dg-do compile }
-- { dg-options "-O -fdump-tree-optimized" }

pragma Overflow_Mode (Minimized);

package body Loop_Optimization22 is

  procedure Foo (X : Discrim_Type) is
      H : array (1 .. Integer (X.Count) + 1) of Float;
   begin
      for I in 1 .. X.Count loop
         H (Integer(I) + 1):= 0.0;
      end loop;
   end;

end Loop_Optimization22;

-- { dg-final { scan-tree-dump-not "Index_Check" "optimized" } }
