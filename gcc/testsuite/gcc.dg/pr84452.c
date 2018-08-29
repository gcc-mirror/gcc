/* PR tree-optimization/84452 */
/* { dg-do compile } */
/* { dg-options "-Ofast" } */

double pow (double, double) __attribute__((simd));
double exp (double) __attribute__((simd));
extern double a[1024], b[1024];

void
foo (void)
{
  for (int i = 0; i < 1024; ++i)
    a[i] = pow (2.0, b[i]);
}
