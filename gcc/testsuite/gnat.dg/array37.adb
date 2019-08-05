-- { dg-do run }
-- { dg-options "-O" }

procedure Array37 is

  type Arr is array (Integer range -1 .. 1) of Integer;

  A : Arr := (-100, 0, 100);

  function Ident (I : Integer) return Integer IS
  begin
    return I;
  end;

begin
  if Ident (A (1)) <= Ident (A (0)) then
    raise Program_Error;
  end if;
end;
