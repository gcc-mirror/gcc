/* PR tree-optimization/69070 */
/* { dg-do compile } */
/* { dg-options "-O2 -fsignaling-nans" } */

double
foo (double d)
{
  return __builtin_pow (d, 2);
}
