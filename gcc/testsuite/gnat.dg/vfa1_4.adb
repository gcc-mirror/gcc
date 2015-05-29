-- { dg-do compile }
-- { dg-options "-fdump-tree-gimple" }

with VFA1_Pkg; use VFA1_Pkg;

procedure VFA1_4 is

  Temp : Int8_t;

  function F (I : Int8_t) return Int8_t is
  begin
    return I;
  end;

  function F2 return Int8_t is
  begin
    return Int8_t(Mixer1(1).R);
  end;

  procedure P3 (I : out Int8_t) is
  begin
    null;
  end;

begin

  Temp := Mixer1(1).R;
  Mixer1(2).R := Temp;

  Temp := Mixer2(1).R;
  Mixer2(2).R := Temp;

  Temp := Mixer1(1).R + Mixer2(2).R;

  if Mixer1(1).R /= Mixer2(2).R then
    raise Program_Error;
  end if;

  Temp := F(Mixer1(1).R);
  Mixer2(2).R := F(Temp);

  Temp := F(Mixer2(2).R);
  Mixer1(1).R := F(Temp);

  Temp := F2;
  P3 (Mixer2(2).R);

end;

-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&vfa1_pkg__mixer1" 7 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&vfa1_pkg__mixer2" 7 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&temp" 0 "gimple"} }

-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&vfa1_pkg__mixer1" 2 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&vfa1_pkg__mixer2" 3 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&temp" 0 "gimple"} }

