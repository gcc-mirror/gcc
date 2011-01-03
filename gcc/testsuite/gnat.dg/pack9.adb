-- { dg-do compile }
-- { dg-options "-O2 -gnatp -fdump-tree-optimized" }
-- See PR tree-optimization/46801 for the expected failure

package body Pack9 is

  procedure Copy (X, Y : R2_Ptr) is
    T : R2 := Y.all;
  begin
    if T.I2 /= Y.I2 then
      raise Program_Error;
    end if;
    X.all := T;
  end;

end Pack9;

-- { dg-final { scan-tree-dump-not "gnat_rcheck" "optimized" } }
-- { dg-final { cleanup-tree-dump "optimized" } }
