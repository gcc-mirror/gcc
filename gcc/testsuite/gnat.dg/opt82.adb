-- { dg-do compile }
-- { dg-options "-O2 -fdump-tree-optimized" }

with Opt82_Pkg; use Opt82_Pkg;

procedure Opt82 (R : access Rec) is
begin
  R.A := 'A';
  R.B := 'B';
  R.C := 'C';
  R.D := 'D';
exception
  when Storage_Error => R.A := 'E';
end;
