-- { dg-do run }
-- { dg-options "-gnatws" }

procedure Alignment9 is

  type Kind is (Small, Large);
  for Kind'Size use 8;

  type Header is
    record
      K : Kind;
      I : Integer;
    end record;

  for Header use
    record
      K at 4 range 0..7;
      I at 0 range 0..31;
    end record;

  for Header'Size use 5*8;
  for Header'Alignment use 1;

  H : Header;

begin
  if H'Size /= 40 then
    raise Program_Error;
  end if;
end;
