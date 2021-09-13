-- { dg-do compile }
-- { dg-options "-O -gnatn -fdump-tree-optimized" }

with Opt94_Pkg; use Opt94_Pkg;

function Opt94 (S : String) return Integer is
  A : constant String := Get;

begin
  if Valid_Result (A) then
    return Result (A);
  else
    return -1;
  end if;
end;

-- { dg-final { scan-tree-dump-times "worker" 1 "optimized" } }
