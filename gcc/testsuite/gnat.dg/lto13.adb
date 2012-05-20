-- { dg-do link }
-- { dg-options "-g -flto" { target lto } }

with Lto13_Pkg; use Lto13_Pkg;

procedure Lto13 is
begin
  Proc;
end;
