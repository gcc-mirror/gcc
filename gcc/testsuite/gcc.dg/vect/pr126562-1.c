/* { dg-do run } */
/* { dg-options "-O3 -fno-math-errno" } */
/* { dg-add-options ieee } */

/* pow (x, 0.5) must not become sqrt (x) while signed zeros are honoured:
   C99 Annex F.10.4.4 gives pow (-0.0, 0.5) == +0.0, while F.10.4.5 gives
   sqrt (-0.0) == -0.0.  */

#include <math.h>

void __attribute__((noinline, noipa))
f (double *restrict d, const double *restrict s, int n)
{
  for (int i = 0; i < n; i++)
    d[i] = pow (s[i], 0.5);
}

int
main (void)
{
  double s[8], d[8];

  for (int i = 0; i < 8; i++)
    s[i] = -0.0;

  f (d, s, 8);

  for (int i = 0; i < 8; i++)
    if (__builtin_signbit (d[i]) != 0)
      __builtin_abort ();

  return 0;
}
