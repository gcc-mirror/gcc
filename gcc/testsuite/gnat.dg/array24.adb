-- { dg-do compile }
-- { dg-options "-fdump-tree-optimized" }

procedure Array24 (N : Natural) is
  S : String (1 .. N);
  pragma Volatile (S);
begin
  S := (others => '0');
end;

-- { dg-final { scan-tree-dump-not "builtin_unwind_resume" "optimized"  } }
