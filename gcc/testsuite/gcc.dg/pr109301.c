/* PR tree-optimization/109301 */
/* { dg-do compile } */
/* { dg-options "-O3 -ffast-math" } */

double x[256];

void
foo (void)
{
  for (int i = 0; i < 256; ++i)
    for (int j = 0; j < 8; ++j)
      x[i] = __builtin_pow (x[i], 0.5);
}
