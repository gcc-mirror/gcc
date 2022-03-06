-- { dg-do compile }
-- { dg-options "-O2 -fdump-tree-optimized" }

with System.Unsigned_Types; use System.Unsigned_Types;

function Opt97 (X, Y : Unsigned) return Unsigned is

  pragma Suppress (All_Checks);

  Z : Unsigned;

begin
  if X >= 2 then
    return 0;
  end if;

  Z := Y;
  if X = 1 then
    Z := Y + 4;
  end if;

  return Z / X;
end;

-- { dg-final { scan-tree-dump "/" "optimized" } }
