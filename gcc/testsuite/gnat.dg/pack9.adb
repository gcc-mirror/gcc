-- { dg-do compile }
-- { dg-options "-O2 -gnatp -cargs --param sra-max-structure-size=24 --param sra-max-structure-count=6 -fdump-tree-final_cleanup" }

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

-- { dg-final { scan-tree-dump-not "gnat_rcheck" "final_cleanup" } }
-- { dg-final { cleanup-tree-dump "final_cleanup" } }
