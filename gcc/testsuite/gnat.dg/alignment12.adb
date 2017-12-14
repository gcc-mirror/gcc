-- { dg-do run }
-- { dg-options "-gnatws" }

procedure Alignment12 is

  type Rec is record
    I : Integer;
  end record;

  R : Rec;
  for R'Alignment use 8;

begin
  if R'Size /= 32 then
    raise Program_Error;
  end if;
end;
