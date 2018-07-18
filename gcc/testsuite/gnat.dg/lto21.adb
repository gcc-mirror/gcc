-- { dg-do run }
-- { dg-options "-O3 -flto" { target lto } }

with Lto21_Pkg1;
with Lto21_Pkg2; use Lto21_Pkg2;

procedure Lto21 is
begin
   Proc;
end;
