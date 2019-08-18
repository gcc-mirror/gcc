--  { dg-do compile }

with Limited2_Pack_2;

procedure Limited2 is
begin
   Limited2_Pack_2.Create (P => Limited2_Pack_2.C1);
end Limited2;
