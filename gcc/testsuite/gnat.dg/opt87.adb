-- { dg-do compile }
-- { dg-require-effective-target store_merge }
-- { dg-options "-O2 -gnatn -fdump-tree-store-merging" }

with Opt87_Pkg; use Opt87_Pkg;

procedure Opt87 is
begin
  Print ("problem detected", "level 1");
end;

-- { dg-final { scan-tree-dump "1 stores to replace old one of 6 stores" "store-merging" } }
