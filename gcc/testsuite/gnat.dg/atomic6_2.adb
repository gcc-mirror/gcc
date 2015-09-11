-- { dg-do compile }
-- { dg-options "-fdump-tree-gimple" }

with Atomic6_Pkg; use Atomic6_Pkg;

procedure Atomic6_2 is
  Temp : Integer;
begin

  Counter1 := Counter1 + Counter2;

  Timer1 := Timer1 + Timer2;

  Counter1 := Counter1 + Int(Timer1);
  Timer1 := Timer1 + Integer(Counter1);

  Temp := Integer(Counter1) + Timer1;
  Counter1 := Int(Timer1) + Int(Temp);
  Timer1 := Integer(Counter1) + Temp;

  if Counter1 /= Counter2 then
    raise Program_Error;
  end if;

  if Timer1 /= Timer2 then
    raise Program_Error;
  end if;

end;

-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&atomic6_pkg__counter1" 6 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&atomic6_pkg__counter2" 2 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&atomic6_pkg__timer1" 6 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&atomic6_pkg__timer2" 2 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&temp" 0 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*ptr" 0 "gimple"} }

-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&atomic6_pkg__counter1" 3 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&atomic6_pkg__counter2" 0 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&atomic6_pkg__timer1" 3 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&atomic6_pkg__timer2" 0 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&temp" 0 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*ptr" 0 "gimple"} }

