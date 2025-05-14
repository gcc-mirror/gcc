/* PR rtl-optimization/117712 */
/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */

int b;

int
foo (int x)
{
  if (b)
    x = 0.96 * x;
  return x;
}
