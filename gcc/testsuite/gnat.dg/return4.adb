-- { dg-do run }
-- { dg-options "-O" }

with Return4_Pkg; use Return4_Pkg;

procedure Return4 is

  type Local_Rec is record
    C : Character;
    R : Rec;
  end record;
  pragma Pack (Local_Rec);

  L : Local_Rec;
  for L'Alignment use 2;

begin
  L.R := Get_Value (0);
  if L.R.I1 /= 0 then
    raise Program_Error;
  end if;
end;
