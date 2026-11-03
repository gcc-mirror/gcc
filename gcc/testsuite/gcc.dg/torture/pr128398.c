/* { dg-do run { target *-*-*gnu* } } */
/* { dg-additional-options "-D_GNU_SOURCE" } */
/* { dg-require-effective-target fenv_exceptions } */

#include <fenv.h>
#include <stdlib.h>

__attribute__((noipa)) void maybe_exit (int c) { if (c) exit (0); }

__attribute__((noipa)) int f (double a, double b, int c)
{
  int r;
  if (c > 5)
    {
      maybe_exit (c);
      r = a < b;
    }
  else
    r = (a < b) * c + 1;
  return r;
}

int main (void)
{
  feenableexcept (FE_INVALID);
  f (__builtin_nan (""), 1.0, 7);
  return 1;
}
