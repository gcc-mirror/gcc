-- { dg-do link }
-- { dg-options "-O -g -flto" { target lto } }

with Lto24_Pkg1;

procedure Lto24 is
  R : Lto24_Pkg1.Rec (False);
begin
  R.Empty := True;
end;
