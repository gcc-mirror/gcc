-- { dg-do compile }
-- { dg-options "-O2 -fdump-tree-optimized" }

function Volatile6 return Integer is

  type Vol is new Integer;
  pragma Volatile (Vol);

  V : Vol := 0;

begin
  for J in 1 .. 10 loop
     V := V + 1;
  end loop;

  return Integer (V);
end;

-- { dg-final { scan-tree-dump "goto" "optimized" } }
-- { dg-final { cleanup-tree-dump "optimized" } }
