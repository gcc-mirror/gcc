-- { dg-do compile }
-- { dg-options "-O2 -fdump-tree-optimized" }

pragma Suppress (Overflow_Check);

function Opt40 (X : Integer; Y : Integer) return Positive is
   Z : Integer;
begin
   if X >= Y then
      return 1;
   end if;
   Z := Y - X;
   return Z;
end;

-- { dg-final { scan-tree-dump-not "gnat_rcheck" "optimized" } }
