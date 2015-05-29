-- { dg-do compile }
-- { dg-options "-fdump-tree-gimple" }

with Atomic6_Pkg; use Atomic6_Pkg;

procedure Atomic6_6 is
  Temp : Integer;
begin

  Counter(1) := Counter(2);

  Timer(1) := Timer(2);

  Counter(1) := Int(Timer(1));
  Timer(1) := Integer(Counter(1));

  Temp := Integer(Counter(1));
  Counter(1) := Int(Temp);

  Temp := Timer(1);
  Timer(1) := Temp;

end;

-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&atomic6_pkg__counter\\\[1" 2 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&atomic6_pkg__counter\\\[2" 1 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&atomic6_pkg__timer\\\[1" 2 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&atomic6_pkg__timer\\\[2" 1 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&temp" 0 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*ptr" 0 "gimple"} }

-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&atomic6_pkg__counter\\\[1" 3 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&atomic6_pkg__counter\\\[2" 0 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&atomic6_pkg__timer\\\[1" 3 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&atomic6_pkg__timer\\\[2" 0 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&temp" 0 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*ptr" 0 "gimple"} }

