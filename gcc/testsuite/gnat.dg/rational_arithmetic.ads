package Rational_Arithmetic is
  -- Whole numbers
  type Whole is new Integer;
--
  -- Undefine unwanted operations
  function "/" (Left, Right: Whole) return Whole is abstract;
--
  -- Rational numbers
--
  type Rational is private;
--
  -- Constructors
--
  function "/" (Left, Right: Whole) return Rational;
--
  -- Rational operations
--
  function "-" (Left, Right: Rational) return Rational;
--
  -- Mixed operations
--
  function "+" (Left: Whole   ; Right: Rational) return Rational;
  function "-" (Left: Whole   ; Right: Rational) return Rational;
  function "-" (Left: Rational; Right: Whole   ) return Rational;
  function "/" (Left: Whole   ; Right: Rational) return Rational;
  function "*" (Left: Whole   ; Right: Rational) return Rational;
  function "*" (Left: Rational; Right: Whole   ) return Rational;
--
  -- Relational
--
  function "=" (Left: Rational; Right: Whole) return Boolean;
--
private
  type Rational is record
    Numerator, Denominator: Whole;
  end record;
end Rational_Arithmetic;
