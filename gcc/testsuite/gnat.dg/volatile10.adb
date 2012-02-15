-- { dg-do compile }

with Volatile10_Pkg; use Volatile10_Pkg;

procedure Volatile10 is
   N : Num;
begin
   N := F.N1;
   N := F.N2;
end;
