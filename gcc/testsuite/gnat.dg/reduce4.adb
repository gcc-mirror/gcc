-- { dg-do compile }
-- { dg-options "-gnat2022" }

procedure Reduce4 (S : String) is
begin
  if [for E of S => 1]'Reduce ("+", 0) = 3 then
    null;
  end if;
end;
