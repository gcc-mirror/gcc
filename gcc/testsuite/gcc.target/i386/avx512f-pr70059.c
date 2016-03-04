/* PR target/70059 */
/* { dg-do run } */
/* { dg-require-effective-target avx512f } */
/* { dg-options "-O2 -mavx512f" } */

#include "avx512f-check.h"

__attribute__((noinline, noclone)) __m512i
foo (__m256i a, __m256i b)
{
  __m512i r = _mm512_undefined_si512 ();
  r = _mm512_inserti64x4 (r, a, 0);
  r = _mm512_inserti64x4 (r, b, 1);
  return r;
}

static void
avx512f_test (void)
{
  union256i_q a, b;
  union512i_q r;
  long long r_ref[8];
  int i;
  for (i = 0; i < 4; i++)
    {
      a.a[i] = 0x0101010101010101ULL * i;
      b.a[i] = 0x1010101010101010ULL * i;
      r_ref[i] = a.a[i];
      r_ref[i + 4] = b.a[i];
    }
  r.x = foo (a.x, b.x);
  check_union512i_q (r, r_ref);
}
