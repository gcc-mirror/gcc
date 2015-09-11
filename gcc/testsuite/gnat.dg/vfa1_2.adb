-- { dg-do compile }
-- { dg-options "-fdump-tree-gimple" }

with VFA1_Pkg; use VFA1_Pkg;

procedure VFA1_2 is
  Temp : Int8_t;

  function F (I : Int8_t) return Int8_t is
  begin
    return I;
  end;

  function F2 return Int8_t is
  begin
    return Int8_t(Timer1(1));
  end;

  procedure P3 (I : out Int8_t) is
  begin
    null;
  end;

begin

  Temp := Timer1(1);
  Timer1(2) := Temp;

  Temp := Timer2(1);
  Timer2(2) := Temp;

  Temp := Timer1(1) + Timer2(2);

  if Timer1(1) /= Timer2(2) then
    raise Program_Error;
  end if;

  Temp := F(Timer1(1));
  Timer2(2) := F(Temp);

  Temp := F(Timer2(2));
  Timer1(1) := F(Temp);

  Temp := F2;
  P3 (Timer2(2));

end;

-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&vfa1_pkg__timer1" 7 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&vfa1_pkg__timer2" 7 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&temp" 0 "gimple"} }

-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&vfa1_pkg__timer1" 2 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&vfa1_pkg__timer2" 3 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&temp" 0 "gimple"} }

