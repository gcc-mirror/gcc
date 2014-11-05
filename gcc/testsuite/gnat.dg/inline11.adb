-- { dg-do compile }
-- { dg-options "-O -gnatn -Winline" }

with Inline11_Pkg; use Inline11_Pkg;

procedure Inline11 is
begin
  Trace (0);
end;
