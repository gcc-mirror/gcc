/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O3" } */

#include "fmla_2.c"

int __attribute__ ((optimize (1)))
main (void)
{
  double a[N], b[N], c[N], d[N], e[N];
  int64_t cond[N];

  for (int i = 0; i < N; ++i)
    {
      c[i] = i + i % 5;
      d[i] = i + i % 7;
      e[i] = i + i % 9;
      cond[i] = i % 3;
    }

  f (a, b, c, d, e, cond);

  for (int i = 0; i < N; ++i)
    if (a[i] != (cond[i] ? __builtin_fma (c[i], d[i], e[i]) : e[i])
	|| b[i] != (cond[i] ? __builtin_fma (c[i], e[i], d[i]) : d[i]))
      __builtin_abort ();

  return 0;
}
