-- { dg-do compile }
-- { dg-options "-O -gnatn -Winline" }

with Inline8_Pkg1; use Inline8_Pkg1;

procedure Inline8 is
begin
  Test (0);
end;
