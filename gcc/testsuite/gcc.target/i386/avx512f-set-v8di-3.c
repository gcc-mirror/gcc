/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

static __m512i
__attribute__ ((noinline))
foo (long long x)
{
  return _mm512_set_epi64 (x, x, x, x, x, x, x, x);
}

static __m512i
__attribute__ ((noinline))
foo_r (long long x)
{
  return _mm512_setr_epi64 (x, x, x, x, x, x, x, x);
}

static void
avx512f_test (void)
{
  int i;
  long long e = 0xfed178ab134badf1LL;
  long long v[8];
  union512i_q res;

  for (i = 0; i < 8; i++)
    v[i] = e;

  res.x = foo (e);

  if (check_union512i_q (res, v))
    abort ();

  res.x = _mm512_setzero_si512 ();

  res.x = foo_r (e);

  if (check_union512i_q (res, v))
    abort ();
}
