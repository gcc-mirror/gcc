/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

static __m512i
__attribute__ ((noinline))
foo (int x, int i)
{
  switch (i)
    {
    case 15:
      return _mm512_set_epi32 (x, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
    case 14:
      return _mm512_set_epi32 (1, x, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
    case 13:
      return _mm512_set_epi32 (1, 1, x, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
    case 12:
      return _mm512_set_epi32 (1, 1, 1, x, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
    case 11:
      return _mm512_set_epi32 (1, 1, 1, 1, x, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
    case 10:
      return _mm512_set_epi32 (1, 1, 1, 1, 1, x, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
    case 9:
      return _mm512_set_epi32 (1, 1, 1, 1, 1, 1, x, 1, 1, 1, 1, 1, 1, 1, 1, 1);
    case 8:
      return _mm512_set_epi32 (1, 1, 1, 1, 1, 1, 1, x, 1, 1, 1, 1, 1, 1, 1, 1);
    case 7:
      return _mm512_set_epi32 (1, 1, 1, 1, 1, 1, 1, 1, x, 1, 1, 1, 1, 1, 1, 1);
    case 6:
      return _mm512_set_epi32 (1, 1, 1, 1, 1, 1, 1, 1, 1, x, 1, 1, 1, 1, 1, 1);
    case 5:
      return _mm512_set_epi32 (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, x, 1, 1, 1, 1, 1);
    case 4:
      return _mm512_set_epi32 (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, x, 1, 1, 1, 1);
    case 3:
      return _mm512_set_epi32 (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, x, 1, 1, 1);
    case 2:
      return _mm512_set_epi32 (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, x, 1, 1);
    case 1:
      return _mm512_set_epi32 (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, x, 1);
    case 0:
      return _mm512_set_epi32 (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, x);
    default:
      abort ();
    }
}

static __m512i
__attribute__ ((noinline))
foo_r (int x, int i)
{
  switch (i)
    {
    case 0:
      return _mm512_setr_epi32 (x, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
    case 1:
      return _mm512_setr_epi32 (1, x, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
    case 2:
      return _mm512_setr_epi32 (1, 1, x, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
    case 3:
      return _mm512_setr_epi32 (1, 1, 1, x, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
    case 4:
      return _mm512_setr_epi32 (1, 1, 1, 1, x, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
    case 5:
      return _mm512_setr_epi32 (1, 1, 1, 1, 1, x, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
    case 6:
      return _mm512_setr_epi32 (1, 1, 1, 1, 1, 1, x, 1, 1, 1, 1, 1, 1, 1, 1, 1);
    case 7:
      return _mm512_setr_epi32 (1, 1, 1, 1, 1, 1, 1, x, 1, 1, 1, 1, 1, 1, 1, 1);
    case 8:
      return _mm512_setr_epi32 (1, 1, 1, 1, 1, 1, 1, 1, x, 1, 1, 1, 1, 1, 1, 1);
    case 9:
      return _mm512_setr_epi32 (1, 1, 1, 1, 1, 1, 1, 1, 1, x, 1, 1, 1, 1, 1, 1);
    case 10:
      return _mm512_setr_epi32 (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, x, 1, 1, 1, 1, 1);
    case 11:
      return _mm512_setr_epi32 (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, x, 1, 1, 1, 1);
    case 12:
      return _mm512_setr_epi32 (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, x, 1, 1, 1);
    case 13:
      return _mm512_setr_epi32 (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, x, 1, 1);
    case 14:
      return _mm512_setr_epi32 (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, x, 1);
    case 15:
      return _mm512_setr_epi32 (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, x);
    default:
      abort ();
    }
}

static void
avx512f_test (void)
{
  int e = 0xabadbeef;
  int v[16];
  union512i_d res;
  int i, j;

  for (i = 0; i < 16; i++)
    {
      for (j = 0; j < 16; j++)
	v[j] = 1;
      v[i] = e;

      res.x = foo (e, i);

      if (check_union512i_d (res, v))
	abort ();

      res.x = _mm512_setzero_si512 ();

      res.x = foo_r (e, i);

      if (check_union512i_d (res, v))
	abort ();
    }
}
