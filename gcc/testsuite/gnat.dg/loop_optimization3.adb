-- { dg-do run }
-- { dg-options "-O" }

with Loop_Optimization3_Pkg; use Loop_Optimization3_Pkg;

procedure Loop_Optimization3 is

  type Arr is array (Integer range -3 .. 3) of Integer;
  C : constant Arr := (1, others => F(2));

begin
  if C /= (1, 2, 2, 2, 2, 2, 2) then
    raise Program_Error;
  end if;
end;
