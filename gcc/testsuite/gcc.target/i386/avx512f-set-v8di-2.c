/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

static __m512i
__attribute__ ((noinline))
foo (long long x1, long long x2, long long x3, long long x4,
     long long x5, long long x6, long long x7, long long x8)
{
  return _mm512_set_epi64 (x1, x2, x3, x4, x5, x6, x7, x8);
}

static __m512i
__attribute__ ((noinline))
foo_r (long long x1, long long x2, long long x3, long long x4,
       long long x5, long long x6, long long x7, long long x8)
{
  return _mm512_setr_epi64 (x8, x7, x6, x5, x4, x3, x2, x1);
}

static void
avx512f_test (void)
{
  long long v[8] = { 0x12e9e94645ad8LL, 0x851c0b39446LL, 2134, 6678,
		     0x786784645245LL, 0x9487731234LL, 41124, 86530 };
  union512i_q res;

  res.x = foo (v[7], v[6], v[5], v[4], v[3], v[2], v[1], v[0]);

  if (check_union512i_q (res, v))
    abort ();

  res.x = _mm512_setzero_si512 ();

  res.x = foo_r (v[7], v[6], v[5], v[4], v[3], v[2], v[1], v[0]);

  if (check_union512i_q (res, v))
    abort ();
}
