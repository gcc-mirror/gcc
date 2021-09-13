/* { dg-do run { target *-*-*gnu* } } */
/* { dg-additional-options "-fno-tree-sink -fno-math-errno -ftree-vectorize -D_GNU_SOURCE" } */
/* { dg-require-effective-target fenv_exceptions } */

#include <fenv.h>

double a[2];
void __attribute__((noipa)) foo ()
{
  double x = a[0];
  double y = a[1];
  double norm = __builtin_sqrt (x*x + y*y);
  if (norm > 1.)
    {
      x = x / norm;
      y = y / norm;
    }
  a[0] = x;
  a[1] = y;
}

int main()
{
  feenableexcept (FE_INVALID);
  a[0] = 0.;
  a[1] = 0.;
  foo ();
  if (a[0] != 0. || a[1] != 0.)
    __builtin_abort ();
  return 0;
}
