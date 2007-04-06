-- { dg-do compile }

with Rational_Arithmetic;
use  Rational_Arithmetic;
procedure Test_Rational_Arithmetic is
  R: Rational := 10/2;
  B: Boolean  := R = 5/1;  -- RHS cannot be a Whole
                           -- ("/" has been "undefined")
  C: Boolean  := R = Rational' (5/1);
  D: Boolean  := (6/3) = R;
  E: Boolean  := (2/1 = 4/2);
begin
  R := 1+1/(4/8);
  R := 2*(3/2)-(7/3)*3;
end Test_Rational_Arithmetic;
