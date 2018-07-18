-- { dg-do compile }
-- { dg-options "-fdump-tree-original" }

package body Renaming6 is

  function Get_I return Integer is
  begin
    return I;
  end;

  procedure Set_I (Val : Integer) is
  begin
    I := Val;
  end;

  function Get_J return Integer is
  begin
    return X;
  end;

  procedure Set_J (Val : Integer) is
  begin
    X := Val;
  end;

end Renaming6;

-- { dg-final { scan-tree-dump-times "atomic_load" 2 "original" } }
-- { dg-final { scan-tree-dump-times "atomic_store" 2 "original" } }
-- { dg-final { scan-tree-dump-not "x" "original" } }
