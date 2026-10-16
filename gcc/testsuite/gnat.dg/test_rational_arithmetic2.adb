-- { dg-do compile }

with Rational_Arithmetic2; use Rational_Arithmetic2;

procedure Test_Rational_Arithmetic2 is
  R: Rational;
  B: Boolean;
  W: Whole;
begin
  R := +5;
  B := R = 5/1; -- { dg-bogus "ambiguous expression" }
  W := 5/1;     -- { dg-error "not directly visible|use clause" }
end Test_Rational_Arithmetic2;
