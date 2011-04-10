-- { dg-do compile { target *-*-linux* } }
-- { dg-options "-gdwarf-2" }

procedure Return3 is
begin
  return;
end;

-- { dg-final { scan-assembler "loc 1 6" } }
