-- { dg-do run }
-- { dg-options "-O2" }

with Casesi;
procedure Test_Casesi is
begin
  Casesi.Try (1);
  Casesi.Try (2);
  Casesi.Try (3);
end;


