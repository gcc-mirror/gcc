/* { dg-do run } */
/* { dg-options "-O2 -fno-strict-aliasing -msse2" } */
/* { dg-additional-options "-mno-mmx" { target { ! ia32 } } } */

#include <string.h>
#include "sse2-check.h"

__attribute__((noinline, noclone))
static void
test_cvtpi32_ps (__m128 *i1, __m64 *i2, __m128 *r)
{
  *(__m128 *) r = _mm_cvtpi32_ps (*i1, *i2);
}

/* Routine to manually compute the results */
static void
compute_correct_result (__m128 *dst_p, __m64 *src_p, __m128 *res_p)
{
  int *src = (int *) src_p;
  float *res = (float *) res_p;
  *res_p = *dst_p;
  int i;
  __m128 r;
  for (i = 0; i < 2; i++)
    {
      r = _mm_cvt_si2ss (*dst_p, src[i]);
      res[i] = ((__v4sf) r)[0];
    }
}

static void
sse2_test (void)
{
  __m128 r, ck;
  __v4sf x = { 1.99f, -3.9f, -4.9f, 3.8f };
  __v2si y = { 30, -39 };

  /* Run the MMX tests */
  test_cvtpi32_ps ((__m128 *) &x, (__m64 *) &y, &r);
  compute_correct_result ((__m128 *) &x, (__m64 *) &y, &ck);
  if (memcmp (&ck, &r, sizeof (r)))
    abort ();
}
