/* PR tree-optimization/107975 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-add-options ieee } */

double
foo (double x, double y)
{
  if (x == 42.0)
    return 1.0;
  double r = x * y;
  if (!__builtin_isnan (r))
    __builtin_unreachable ();
  return r;
}
