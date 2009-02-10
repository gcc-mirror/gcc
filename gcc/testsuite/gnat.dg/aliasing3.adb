-- { dg-do run }
-- { dg-options "-O2 -gnatn" }

with Aliasing3_Pkg; use Aliasing3_Pkg;

procedure Aliasing3 is
begin
  Pointer.A(1) := 5;
  Test (Block.A);
end;
