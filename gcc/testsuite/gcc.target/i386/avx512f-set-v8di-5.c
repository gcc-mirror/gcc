/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

static __m512i
__attribute__ ((noinline))
foo (long long x, int i)
{
  switch (i)
    {
    case 7:
      return _mm512_set_epi64 (x, 1, 1, 1, 1, 1, 1, 1);
    case 6:
      return _mm512_set_epi64 (1, x, 1, 1, 1, 1, 1, 1);
    case 5:
      return _mm512_set_epi64 (1, 1, x, 1, 1, 1, 1, 1);
    case 4:
      return _mm512_set_epi64 (1, 1, 1, x, 1, 1, 1, 1);
    case 3:
      return _mm512_set_epi64 (1, 1, 1, 1, x, 1, 1, 1);
    case 2:
      return _mm512_set_epi64 (1, 1, 1, 1, 1, x, 1, 1);
    case 1:
      return _mm512_set_epi64 (1, 1, 1, 1, 1, 1, x, 1);
    case 0:
      return _mm512_set_epi64 (1, 1, 1, 1, 1, 1, 1, x);
    default:
      abort ();
    }
}

static __m512i
__attribute__ ((noinline))
foo_r (long long x, int i)
{
  switch (i)
    {
    case 0:
      return _mm512_setr_epi64 (x, 1, 1, 1, 1, 1, 1, 1);
    case 1:
      return _mm512_setr_epi64 (1, x, 1, 1, 1, 1, 1, 1);
    case 2:
      return _mm512_setr_epi64 (1, 1, x, 1, 1, 1, 1, 1);
    case 3:
      return _mm512_setr_epi64 (1, 1, 1, x, 1, 1, 1, 1);
    case 4:
      return _mm512_setr_epi64 (1, 1, 1, 1, x, 1, 1, 1);
    case 5:
      return _mm512_setr_epi64 (1, 1, 1, 1, 1, x, 1, 1);
    case 6:
      return _mm512_setr_epi64 (1, 1, 1, 1, 1, 1, x, 1);
    case 7:
      return _mm512_setr_epi64 (1, 1, 1, 1, 1, 1, 1, x);
    default:
      abort ();
    }
}

static void
avx512f_test (void)
{
  long long e = 0xabadbeef01234567LL;
  long long v[8];
  union512i_q res;
  int i, j;

  for (i = 0; i < 8; i++)
    {
      for (j = 0; j < 8; j++)
	v[j] = 1;
      v[i] = e;

      res.x = foo (e, i);

      if (check_union512i_q (res, v))
	abort ();

      res.x = _mm512_setzero_si512 ();

      res.x = foo_r (e, i);

      if (check_union512i_q (res, v))
	abort ();
    }
}
