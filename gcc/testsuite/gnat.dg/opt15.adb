-- { dg-do compile }
-- { dg-options "-O -gnatn -fdump-tree-optimized" }

with Opt15_Pkg; use Opt15_Pkg;

procedure Opt15 is
begin
  Trace_Inlined;
end;

-- { dg-final { scan-tree-dump-not "trace_inlined" "optimized" } }
