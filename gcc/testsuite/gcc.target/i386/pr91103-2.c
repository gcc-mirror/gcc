/* { dg-do run } */
/* { dg-options "-O2 -mavx512vl" } */
/* { dg-require-effective-target avx512vl } */

#define AVX512VL

#ifndef CHECK
#define CHECK "avx512f-helper.h"
#endif

#include CHECK
#include "pr91103-1.c"

#define RUNCHECK(U,V,S,IDX)			\
  do						\
    {						\
      S tmp = foo_##V##_##IDX ((V)U.x);		\
      if (tmp != U.a[IDX])			\
	abort();				\
    }						\
  while (0)

void
test_256 (void)
{
  union512i_d di1;
  union256i_d di2;
  union512i_q q1;
  union256i_q q2;
  union512 f1;
  union256 f2;
  union512d d1;
  union256d d2;
  int sign = 1;

  int i = 0;
  for (i = 0; i < 16; i++)
    {
      di1.a[i] = 30 * (i - 30) * sign;
      f1.a[i] = 56.78 * (i - 30) * sign;
      sign = -sign;
    }

  for (i = 0; i != 8; i++)
    {
      di2.a[i] = 15 * (i + 40) * sign;
      f2.a[i] = 90.12 * (i + 40) * sign;
      q1.a[i] = 15 * (i + 40) * sign;
      d1.a[i] = 90.12 * (i + 40) * sign;
      sign = -sign;
    }

  for (i = 0; i != 4; i++)
    {
      q2.a[i] = 15 * (i + 40) * sign;
      d2.a[i] = 90.12 * (i + 40) * sign;
      sign = -sign;
    }

RUNCHECK (f2, v8sf, float, 4);
RUNCHECK (f2, v8sf, float, 7);
RUNCHECK (di2, v8si, int, 4);
RUNCHECK (di2, v8si, int, 7);
RUNCHECK (f1, v16sf, float, 4);
RUNCHECK (f1, v16sf, float, 8);
RUNCHECK (f1, v16sf, float, 12);
RUNCHECK (f1, v16sf, float, 15);
RUNCHECK (di1, v16si, int, 4);
RUNCHECK (di1, v16si, int, 8);
RUNCHECK (di1, v16si, int, 12);
RUNCHECK (di1, v16si, int, 15);
RUNCHECK (d2, v4df, double, 2);
RUNCHECK (d2, v4df, double, 3);
RUNCHECK (q2, v4di, long long, 2);
RUNCHECK (q2, v4di, long long, 3);
RUNCHECK (d1, v8df, double, 4);
RUNCHECK (d1, v8df, double, 7);
RUNCHECK (q1, v8di, long long, 4);
RUNCHECK (q1, v8di, long long, 7);
}

void
test_128()
{
}
