-- { dg-do compile }
-- { dg-options "-O -gnatn -Winline" }

with Inline6_Pkg; use Inline6_Pkg;

procedure Inline6 is
begin
  Test (0);
end;
