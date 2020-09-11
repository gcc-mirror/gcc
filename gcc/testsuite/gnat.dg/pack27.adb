-- { dg-do compile }

with Pack27_Pkg; use Pack27_Pkg;

procedure Pack27 is
  R1 : Rec1;
  R4 : Rec4;
begin
  R4.R.R.R := R1;
end;
