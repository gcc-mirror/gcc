/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

static __m512d
__attribute__ ((noinline))
foo (double x, int i)
{
  switch (i)
    {
    case 7:
      return _mm512_set_pd (x, 1, 1, 1, 1, 1, 1, 1);
    case 6:
      return _mm512_set_pd (1, x, 1, 1, 1, 1, 1, 1);
    case 5:
      return _mm512_set_pd (1, 1, x, 1, 1, 1, 1, 1);
    case 4:
      return _mm512_set_pd (1, 1, 1, x, 1, 1, 1, 1);
    case 3:
      return _mm512_set_pd (1, 1, 1, 1, x, 1, 1, 1);
    case 2:
      return _mm512_set_pd (1, 1, 1, 1, 1, x, 1, 1);
    case 1:
      return _mm512_set_pd (1, 1, 1, 1, 1, 1, x, 1);
    case 0:
      return _mm512_set_pd (1, 1, 1, 1, 1, 1, 1, x);
    default:
      abort ();
    }
}

static __m512d
__attribute__ ((noinline))
foo_r (double x, int i)
{
  switch (i)
    {
    case 0:
      return _mm512_setr_pd (x, 1, 1, 1, 1, 1, 1, 1);
    case 1:
      return _mm512_setr_pd (1, x, 1, 1, 1, 1, 1, 1);
    case 2:
      return _mm512_setr_pd (1, 1, x, 1, 1, 1, 1, 1);
    case 3:
      return _mm512_setr_pd (1, 1, 1, x, 1, 1, 1, 1);
    case 4:
      return _mm512_setr_pd (1, 1, 1, 1, x, 1, 1, 1);
    case 5:
      return _mm512_setr_pd (1, 1, 1, 1, 1, x, 1, 1);
    case 6:
      return _mm512_setr_pd (1, 1, 1, 1, 1, 1, x, 1);
    case 7:
      return _mm512_setr_pd (1, 1, 1, 1, 1, 1, 1, x);
    default:
      abort ();
    }
}

static void
avx512f_test (void)
{
  double e = -3.234;
  double v[8];
  union512d res;
  int i, j;

  for (i = 0; i < 8; i++)
    {
      for (j = 0; j < 8; j++)
	v[j] = 1;
      v[i] = e;

      res.x = foo (e, i);

      if (check_union512d (res, v))
	abort ();

      res.x = _mm512_setzero_pd ();

      res.x = foo_r (e, i);

      if (check_union512d (res, v))
	abort ();
    }
}
