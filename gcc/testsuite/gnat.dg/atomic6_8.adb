-- { dg-do compile }
-- { dg-options "-fdump-tree-gimple" }

with Atomic6_Pkg; use Atomic6_Pkg;

procedure Atomic6_8 is
  Ptr : Int_Ptr := new Int;
  Temp : Integer;
begin

  Ptr.all := Counter1;

  Counter1 := Ptr.all;

  Ptr.all := Int(Timer1);
  Timer1 := Integer(Ptr.all);

  Temp := Integer(Ptr.all);
  Ptr.all := Int(Temp);

end;

-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&atomic6_pkg__counter1" 1 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&atomic6_pkg__counter2" 0 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&atomic6_pkg__timer1" 1 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&atomic6_pkg__timer2" 0 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&temp" 0 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*ptr" 3 "gimple"} }

-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&atomic6_pkg__counter1" 1 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&atomic6_pkg__counter2" 0 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&atomic6_pkg__timer1" 1 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&atomic6_pkg__timer2" 0 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&temp" 0 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*ptr" 3 "gimple"} }

