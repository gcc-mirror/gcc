// { dg-additional-options "-fmodules-ts" }
export module sqrt;
// { dg-module-cmi "sqrt" }

export constexpr unsigned sqrt (unsigned X, unsigned x = 1)
{
  // Newton-Raphson, not binary restoring
  // x <= x - f(x)/f'(x)
  // f(x) = x^2 - X
  // f'(x) = 2x
  // x <= x - (x^2 - X) / 2x
  // x <= x - x/2 + X/2x
  // x <= x/2 + X/2x
  // x <= 1/2(x + X/x)
  unsigned nx = (x + X/x) / 2;
  if (nx != x)
    nx = sqrt (X, nx);
  return nx;
}
