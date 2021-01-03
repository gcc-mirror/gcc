-- { dg-do compile }
-- { dg-options "-gnatws" }

procedure Shift1 is

  type T_Integer_8 is range -2 ** 7 .. 2 ** 7 - 1
    with Size => 8;

  pragma Provide_Shift_Operators (T_Integer_8);

  X : T_Integer_8;

begin
  X := Shift_Right (X, 1);
end;
