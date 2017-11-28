-- { dg-options "-fprofile-arcs -ftest-coverage" }
-- { dg-do run { target native } } */

procedure Check is

  function Add1 (I1, I2 : Integer) return Integer is
  begin
    return I1 + I2;  -- count(1)
  end;

  function Add2 (I1, I2 : Integer) return Integer is
    pragma Suppress (Overflow_Check);
  begin
    return I1 + I2;  -- count(1)
  end;

begin
  if Add1 (1, 2) /= 3 then
    raise Program_Error;
  end if;

  if Add2 (1, 2) /= 3 then
    raise Program_Error;
  end if;
end;

-- { dg-final { run-gcov check.adb } }
