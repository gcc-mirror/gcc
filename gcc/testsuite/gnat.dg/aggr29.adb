-- { dg-do compile }
-- { dg-options "-fdump-tree-gimple" }

with Ada.Streams; use Ada.Streams;

procedure Aggr29 is
  A : aliased Stream_Element_Array := (1 .. 512 => <>);
begin
  null;
end;

-- { dg-final { scan-tree-dump-not "a___UNC = \\*" "gimple" } }
