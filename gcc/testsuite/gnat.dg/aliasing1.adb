-- { dg-do compile }
-- { dg-options "-O2 -gnatp -fdump-tree-optimized" }

-- The raise statement must be optimized away by
-- virtue of DECL_NONADDRESSABLE_P set on R.I.

package body Aliasing1 is

  function F (P : Ptr) return Integer is
  begin
    R.I := 0;
    P.all := 1;
    if R.I /= 0 then
      raise Program_Error;
    end if;
    return 0;
  end;

end Aliasing1;

-- { dg-final { scan-tree-dump-not "gnat_rcheck" "optimized" } }
-- { dg-final { cleanup-tree-dump "optimized" } }
