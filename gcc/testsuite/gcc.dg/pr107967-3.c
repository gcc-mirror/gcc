/* PR tree-optimization/107967 */
/* { dg-do compile { target float64 } } */
/* { dg-options "-O2 -fno-rounding-math -fno-trapping-math -fdump-tree-optimized" } */
/* { dg-add-options float64 } */
/* { dg-final { scan-tree-dump-times "return\[ \t]\*-?Inf;" 3 "optimized" } } */

_Float64
foo (_Float64 x)
{
  if (x >= 1.0e+300f64)
    ;
  else
    __builtin_unreachable ();
  return x * x;
}

_Float64
bar (_Float64 x)
{
  if (x >= 1.0e+300f64)
    ;
  else
    __builtin_unreachable ();
  return x * -x;
}

_Float64
baz (_Float64 a, _Float64 b)
{
  if (a >= 0x1.fffffffffffffp+1023f64)
    ;
  else
    __builtin_unreachable ();
  if (b >= 0x1.p+972f64)
    ;
  else
    __builtin_unreachable ();
  return a + b;
}

_Float64
qux (_Float64 a, _Float64 b)
{
  if (a >= 0x1.fffffffffffffp+1023f64)
    ;
  else
    __builtin_unreachable ();
  if (b >= 0x1.fffffffffffffp+969f64)
    ;
  else
    __builtin_unreachable ();
  return a + b;
}
