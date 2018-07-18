-- { dg-do run }
-- { dg-options "-gnatws" }

procedure Alignment13 is

  type Rec is record
    I1 : aliased Short_Integer;
    I2 : Integer;
  end record;

  for Rec use record
    I1 at 0 range 0 .. 15;
  end record;

  R : Rec;

begin
  if R.I2'Bit_Position /= 32 then
    raise Program_Error;
  end if;
end;
