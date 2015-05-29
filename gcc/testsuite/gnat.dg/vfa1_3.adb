-- { dg-do compile }
-- { dg-options "-fdump-tree-gimple" }

with VFA1_Pkg; use VFA1_Pkg;

procedure VFA1_3 is

  Temp : Short_Integer;

  function F (I : Short_Integer) return Short_Integer is
  begin
    return I;
  end;

  function F2 return Short_Integer is
  begin
    return Short_Integer(Buffer1.A);
  end;

  procedure P3 (I : out Short_Integer) is
  begin
    null;
  end;

begin

  Temp := Buffer1.A;
  Buffer1.B := Temp;

  Temp := Buffer2.A;
  Buffer2.B := Temp;

  Temp := Buffer1.A + Buffer2.B;

  if Buffer1.A /= Buffer2.B then
    raise Program_Error;
  end if;

  Temp := F(Buffer1.A);
  Buffer2.B := F(Temp);

  Temp := F(Buffer2.B);
  Buffer1.A := F(Temp);

  Temp := F2;
  P3 (Buffer2.B);

end;

-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&vfa1_pkg__buffer1" 7 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&vfa1_pkg__buffer2" 7 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_load\[^\n\r\]*&temp" 0 "gimple"} }

-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&vfa1_pkg__buffer1" 2 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&vfa1_pkg__buffer2" 3 "gimple"} }
-- { dg-final { scan-tree-dump-times "atomic_store\[^\n\r\]*&temp" 0 "gimple"} }

