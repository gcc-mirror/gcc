-- { dg-do compile }
-- { dg-options "-fdump-tree-gimple" }

with Atomic6_Pkg; use Atomic6_Pkg;

procedure Atomic6_5 is
  type Arr is array (Integer range 1 .. 4) of Boolean;
  A : Arr;
  B : Boolean;
begin

  A (Integer(Counter1)) := True;
  B := A (Timer1);

  declare
    pragma Suppress (Index_Check);
  begin
    A (Integer(Counter1)) := True;
    B := A (Timer1);
  end;

end;

-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&atomic6_pkg__counter1" 2 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&atomic6_pkg__counter2" 0 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&atomic6_pkg__timer1" 2 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&atomic6_pkg__timer2" 0 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&temp" 0 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*ptr" 0 "gimple"} }

-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&atomic6_pkg__counter1" 0 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&atomic6_pkg__counter2" 0 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&atomic6_pkg__timer1" 0 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&atomic6_pkg__timer2" 0 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&temp" 0 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*ptr" 0 "gimple"} }

