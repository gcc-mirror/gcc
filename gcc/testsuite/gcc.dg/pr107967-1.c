/* PR tree-optimization/107967 */
/* { dg-do compile { target float64 } } */
/* { dg-options "-O2 -frounding-math -fno-trapping-math -fdump-tree-optimized" } */
/* { dg-add-options float64 } */
/* { dg-final { scan-tree-dump-not "return\[ \t]\*-?Inf;" "optimized" } } */

_Float64
foo (void)
{
  const _Float64 huge = 1.0e+300f64;
  return huge * huge;
}

_Float64
bar (void)
{
  const _Float64 huge = 1.0e+300f64;
  return huge * -huge;
}

_Float64
baz (void)
{
  const _Float64 a = 0x1.fffffffffffffp+1023f64;
  const _Float64 b = 0x1.fffffffffffffp+970f64;
  return a + b;
}

_Float64
qux (void)
{
  const _Float64 a = 0x1.fffffffffffffp+1023f64;
  const _Float64 b = 0x1.fffffffffffffp+969f64;
  return a + b;
}
