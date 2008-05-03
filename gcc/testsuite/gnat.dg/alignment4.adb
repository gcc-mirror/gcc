-- { dg-do compile }
-- { dg-options "-gnatws -fdump-tree-gimple" }

procedure Alignment4 is

  type Stream is array (1..3) of Character;

  S1, S2 : Stream;

begin
  S1 := S2;
end;

-- { dg-final { scan-tree-dump-not ".\F" "gimple" } }
-- { dg-final { cleanup-tree-dump "gimple" } }
