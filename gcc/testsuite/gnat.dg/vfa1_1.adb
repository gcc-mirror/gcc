-- { dg-do compile }
-- { dg-options "-fdump-tree-gimple" }

with VFA1_Pkg; use VFA1_Pkg;

procedure VFA1_1 is
  Temp : Integer;

  function F (I : Integer) return Integer is
  begin
    return I;
  end;

  function F2 return Integer is
  begin
    return Integer(Counter1);
  end;

  procedure P3 (I : Out Integer) is
  begin
    null;
  end;

begin

  Counter1 := Int(Counter2);
  Counter2 := Integer(Counter1);

  Temp := Integer(Counter1);
  Counter1 := Int(Temp);

  Temp := Counter2;
  Counter2 := Temp;

  Temp := Integer (Counter1) + Counter2;

  if Counter1 /= Int (Counter2) then
    raise Program_Error;
  end if;

  Temp := F(Integer (Counter1));
  Counter1 := Int(F(Temp));

  Temp := F(Counter2);
  Counter2 := F(Temp);

  Temp := F2;
  P3 (Counter2);

end;

-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&vfa1_pkg__counter1" 6 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&vfa1_pkg__counter2" 5 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&temp" 0 "gimple"} }

-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&vfa1_pkg__counter1" 3 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&vfa1_pkg__counter2" 4 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&temp" 0 "gimple"} }

