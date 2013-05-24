-- { dg-do compile }
-- { dg-options "-O2 -fdump-tree-optimized" }

package body Noinline2 is

  function Inner (A, B : Integer) return Integer;
  pragma No_Inline (Inner);

  function Inner (A, B : Integer) return Integer is
  begin
    return A + B;
  end;

  function F (A, B : Integer) return Integer is
  begin
    return Inner (A, B) + Inner (A, -B);
  end;

end Noinline2;

-- { dg-final { scan-tree-dump-times "noinline2.inner" 2 "optimized"  } }
-- { dg-final { cleanup-tree-dump "optimized" } }
