--  { dg-do run }

procedure Array34 is

  type Arr is array (1 .. 6) of Short_Short_Integer;
  for Arr'Alignment use 4;

  type Rec is record
    A : Arr;
    B: Short_Integer;
  end record;
  pragma Pack (Rec);

  R : Rec;

begin
  R.B := 31415;
  R.A := (others => 0);
  if R.B /= 31415 then
    raise Program_Error;
  end if;
end;
