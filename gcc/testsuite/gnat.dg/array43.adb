-- { dg-do run }

procedure Array43 is

  type Arr is array (Integer range <>) of Integer;

  procedure Fill (B : out Arr; N : Integer) is
  begin
    B := (1 .. N => 7);
  end Fill;

  V : Arr (5 .. 8) := (others => 0);

begin
  Fill (V, V'Length);
  if V /= (V'First .. V'Last => 7) then
    raise Program_Error;
  end if;
end;
