-- { dg-do run }

procedure Bias2 is

  type Biased_T is range 1 .. 2 ** 6;
  for Biased_T'Size use 6;  --  { dg-warning "biased representation" }
  X, Y : Biased_T;

begin
  X := 1;
  Y := 1;
  if X + Y /= 2 then
    raise Program_Error;
  end if;

  X := 2;
  Y := 1;
  if X - Y /= 1 then
    raise Program_Error;
  end if;

  X := 2;
  Y := 3;
  if X * Y /= 6 then
    raise Program_Error;
  end if;

  X := 24;
  Y := 3;
  if X / Y /= 8 then
    raise Program_Error;
  end if;
end;
