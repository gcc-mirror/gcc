-- { dg-do compile }
-- { dg-options "-O2 -gnatp -fdump-tree-final_cleanup" }

-- The raise statement must be optimized away by
-- virtue of TYPE_NONALIASED_COMPONENT set on A.

package body Aliasing2 is

  function F (P : Ptr) return Integer is
  begin
    A (1) := 0;
    P.all := 1;
    if A(1) /= 0 then
      raise Program_Error;
    end if;
    return 0;
  end;

end Aliasing2;

-- { dg-final { scan-tree-dump-not "gnat_rcheck" "final_cleanup" } }
-- { dg-final { cleanup-tree-dump "final_cleanup" } }
