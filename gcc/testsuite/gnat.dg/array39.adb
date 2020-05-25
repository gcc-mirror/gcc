-- { dg-do run }

with Array39_Pkg; use Array39_Pkg;

procedure Array39 is
  T : Tsk;
  R : Rec2;
begin
  T.E (R, 1);
  if R.A (1) /= Val then
    raise Program_Error;
  end if;
end;
