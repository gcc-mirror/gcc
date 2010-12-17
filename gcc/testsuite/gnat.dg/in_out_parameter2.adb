-- { dg-do run }
-- { dg-options "-gnat12" }

procedure In_Out_Parameter2 is

  function F (I : In Out Integer) return Boolean is
    A : Integer := I;
  begin
    I := I + 1;
    return (A > 0);
  end;

  I : Integer := 0;
  B : Boolean;

begin
  B := F (I);
  if B then
    raise Program_Error;
  end if;
  if I /= 1 then
    raise Program_Error;
  end if;
end;
