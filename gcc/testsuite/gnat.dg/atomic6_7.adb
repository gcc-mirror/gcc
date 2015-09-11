-- { dg-do compile }
-- { dg-options "-fdump-tree-gimple" }

with Atomic6_Pkg; use Atomic6_Pkg;

procedure Atomic6_7 is
  My_Atomic  : R;
  Temp : Integer;
begin

  My_Atomic.Counter1 := Counter2;

  My_Atomic.Timer1 := Timer2;

  My_Atomic.Counter1 := Int(My_Atomic.Timer1);
  My_Atomic.Timer1 := Integer(My_Atomic.Counter1);

  Temp := Integer(My_Atomic.Counter1);
  My_Atomic.Counter1 := Int(Temp);

  Temp := My_Atomic.Timer1;
  My_Atomic.Timer1 := Temp;

end;

-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&my_atomic.counter1" 2 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&atomic6_pkg__counter2" 1 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&my_atomic.timer1" 2 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&atomic6_pkg__timer2" 1 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&temp" 0 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*ptr" 0 "gimple"} }

-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&my_atomic.counter1" 3 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&atomic6_pkg__counter2" 0 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&my_atomic.timer1" 3 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&atomic6_pkg__timer2" 0 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&temp" 0 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*ptr" 0 "gimple"} }

