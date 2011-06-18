-- { dg-do compile }
-- { dg-options "-O2 -fdump-tree-optimized" }

function Volatile8 return Integer is

   type Vol is new Integer;
   pragma Volatile (Vol);

   type A is array (1..4) of Vol;

   V : A := (others => 0);

begin
   for J in 1 .. 10 loop
      V(1) := V(1) + 1;
   end loop;

   return Integer (V(1));
end;

-- { dg-final { scan-tree-dump "goto" "optimized" } }
-- { dg-final { cleanup-tree-dump "optimized" } }
