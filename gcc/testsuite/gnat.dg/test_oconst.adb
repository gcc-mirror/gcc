--  { dg-do run }

with OCONST1, OCONST2, OCONST3, OCONST4, OCONST5;

procedure Test_Oconst is
begin
  OCONST1.check (OCONST1.My_R);
  OCONST2.check (OCONST2.My_R);
  OCONST3.check (OCONST3.My_R);
  OCONST4.check (OCONST4.My_R);
  OCONST5.check (OCONST5.My_R0, 0);
  OCONST5.check (OCONST5.My_R1, 1);
end;
