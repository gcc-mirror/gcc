/* PR tree-optimization/96715 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "= __builtin_copysign" "optimized" } } */
/* { dg-final { scan-tree-dump-times " = -x_\[0-9]*\\(D\\)" 3 "optimized" } } */

float
foo (float x)
{
  return __builtin_copysignf (x, -x);
}

double
bar (double x)
{
  return __builtin_copysign (x, -x);
}

long double
baz (long double x)
{
  return __builtin_copysignl (x, -x);
}
