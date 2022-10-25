/* PR tree-optimization/107368 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

double
f4 (double x)
{
  [[gnu::assume (x && x > 0.0)]];
  return x;
}
