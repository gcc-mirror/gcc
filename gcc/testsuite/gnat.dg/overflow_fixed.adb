-- { dg-do run }
-- { dg-options "-gnato -O" }

procedure Overflow_Fixed is

  type Unsigned_8_Bit is mod 2**8;

  procedure Fixed_To_Eight (Value : Duration) is
    Item : Unsigned_8_Bit;
  begin
    Item := Unsigned_8_Bit(Value);
    raise Program_Error;
  exception
    when Constraint_Error => null; -- expected case
  end;

begin
  Fixed_To_Eight (-0.5);
end;
