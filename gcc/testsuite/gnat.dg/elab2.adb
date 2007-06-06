--  { dg-do compile }
--  { dg-options "-gnatws" }

with elab1;

procedure elab2 is
  A : elab1.My_Rec;
begin
  null;
end;
