-- { dg-do compile }
-- { dg-options "-O -gnatn -Winline" }

with Inline10_Pkg; use Inline10_Pkg;

procedure Inline10 is
begin
  Test (0);
end;
