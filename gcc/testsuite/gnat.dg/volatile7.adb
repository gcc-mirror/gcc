-- { dg-do compile }
-- { dg-options "-O2 -fdump-tree-optimized" }

function Volatile7 return Integer is

   type Vol is new Integer;
   pragma Volatile (Vol);

   type R is record
      X : Vol := 0;
   end record;

   V : R;

begin
   for J in 1 .. 10 loop
      V.X := V.X + 1;
   end loop;

   return Integer (V.X);
end;

-- { dg-final { scan-tree-dump "goto" "optimized" } }
-- { dg-final { cleanup-tree-dump "optimized" } }
