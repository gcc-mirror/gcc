-- { dg-do run }
-- { dg-options "-gnat2022" }

procedure Aggr36 is

  package Nested is
    X : constant Integer := 0;

    generic
      L : Positive;
    package G is
      type Arr is array (1 .. L) of Integer;
      
      A : constant Arr := [for I in 1 .. L => X + I];
      B : constant Arr := [A with delta for I in 1 .. L/2 => X + 2 * I];
    end G;
  end Nested;

  X : constant Integer := 1000;

  package My_G is new Nested.G (L => 10);

begin
  if My_G.A (1) /= 1 then
    raise Program_Error;
  end if;
  if My_G.B (1) /= 2 then
    raise Program_Error;
  end if;
end;
