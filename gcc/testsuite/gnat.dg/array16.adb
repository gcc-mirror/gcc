-- { dg-do compile }
-- { dg-options "-O -gnatn -fdump-tree-optimized" }

package body Array16 is

  function F1 (A : access My_T1) return My_T1 is
  begin
    return A.all;
  end;

  function F2 (A : access My_T2) return My_T2 is
  begin
    return A.all;
  end;

  procedure Proc (A : access My_T1; B : access My_T2) is
    L1 : My_T1 := F1(A);
    L2 : My_T2 := F2(B);
  begin
    if L1.D = 0 and then L2(1) = 0 then
      raise Program_Error;
    end if;
  end;

end Array16;

-- { dg-final { scan-tree-dump-not "secondary_stack" "optimized" } }
