-- { dg-do run }
-- { dg-options "-gnatws" }

procedure Alignment11 is

  type Arr is array (1 .. 3) of Character;
  for Arr'Alignment use 4;

  A : Arr;

begin
  if A'Size /= 32 then
    raise Program_Error;
  end if;
end;
