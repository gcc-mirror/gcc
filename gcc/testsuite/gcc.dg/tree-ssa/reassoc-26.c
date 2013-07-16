/* PR tree-optimization/56559 */
/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */

double a, b, c, d, e;

void
foo ()
{
  a = e * e;
  b = d * e + c * e * a;
}
