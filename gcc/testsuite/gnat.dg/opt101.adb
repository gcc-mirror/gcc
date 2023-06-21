-- { dg-do run }
-- { dg-options "-O" }

pragma Optimize_Alignment (Space);

with Opt101_Pkg; use Opt101_Pkg;

procedure Opt101 is

  C1 : Cont1;
  C2 : Cont2;

begin
  C1 := ((1234, 1, 2), 1, 2);
  if C1.R.I1 /= 1 or C1.I2 /= 2 then
    raise Program_Error;
  end if;

  C2 := (1, (1234, 1, 2), 2);
  if C2.R.I1 /= 1 or C2.I2 /= 2 then
    raise Program_Error;
  end if;
end;
