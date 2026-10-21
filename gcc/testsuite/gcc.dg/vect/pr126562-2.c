/* { dg-do run } */
/* { dg-options "-O3 -fno-math-errno" } */
/* { dg-add-options ieee } */

/* pow (x, 0.5) must not become sqrt (x) while infinities are honoured:
   C99 Annex F.10.4.4 gives pow (-Inf, 0.5) == +Inf, while F.10.4.5 gives
   sqrt (-Inf) == NaN, with the invalid exception raised.  */

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
    s[i] = 4.0;
  s[3] = -__builtin_inf ();

  f (d, s, 8);

  if (d[3] != __builtin_inf ())
    __builtin_abort ();

  return 0;
}

