with Invisible;

package Rational_Arithmetic2 is

  subtype Whole is Invisible.Whole;

  type Rational is private;

  function "+" (Right: Whole) return Rational;

  function "/" (Left, Right: Whole) return Rational;

  function "=" (Left: Rational; Right: Whole) return Boolean;

private

  type Rational is record
    Numerator  : Whole;
    Denominator: Whole;
  end record;

end Rational_Arithmetic2;
