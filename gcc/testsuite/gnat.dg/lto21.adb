-- { dg-do run }
-- { dg-options "-O3 -flto" { target lto } }
-- { dg-prune-output "warning: using serial compilation" }

with Lto21_Pkg1;
with Lto21_Pkg2; use Lto21_Pkg2;

procedure Lto21 is
begin
   Proc;
end;
