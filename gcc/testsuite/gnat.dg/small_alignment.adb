-- { dg-do run }
-- { dg-options "-gnatws" }

procedure Small_Alignment is

  type My_Integer is new Integer;
  for My_Integer'Alignment use 1;

  function Set_A return My_Integer is
  begin
    return 12;
  end;

  function Set_B return My_Integer is
  begin
    return 6;
  end;

  C : Character;
  A : My_Integer := Set_A;
  B : My_Integer := Set_B;

begin
  A := A * B / 2;
  if A /= 36 then
    raise Program_Error;
  end if;
end;
