-- { dg-do compile }
-- { dg-options "-O2 -fdump-tree-optimized" }

function Opt54 (Val, Max : Integer) return Integer is
begin
   if Val >= Max then
      return Max;
   end if;
   return Val + 1;
end;

-- { dg-final { scan-tree-dump-not "gnat_rcheck" "optimized" } }
