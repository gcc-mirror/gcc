-- { dg-do compile }
-- { dg-options "-O2 -fdump-tree-optimized" }

function Opt53 (Val, Max : Positive) return Positive is
begin
   if Val >= Max then
      return Max;
   end if;
   return Val + 1;
end;

-- { dg-final { scan-tree-dump-not "gnat_rcheck" "optimized" } }
