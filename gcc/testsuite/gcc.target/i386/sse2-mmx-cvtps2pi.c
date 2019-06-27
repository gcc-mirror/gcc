/* { dg-do run } */
/* { dg-options "-O2 -fno-strict-aliasing -msse2" } */
/* { dg-additional-options "-mno-mmx" { target { ! ia32 } } } */

#include "sse2-check.h"

__attribute__((noinline, noclone))
static void
test_cvtps_pi32 (__m128 *src_p, long long *r)
{
  *(__m64 *) r = _mm_cvtps_pi32 (*src_p);
}

/* Routine to manually compute the results */
static void
compute_correct_result (__m128 *src_p, long long *res_p)
{
  __v4sf *src = (__v4sf *) src_p;
  int *res = (int *) res_p;
  int i;
  for (i = 0; i < 2; i++)
    res[i] = _mm_cvt_ss2si (_mm_set_ss ((*src)[i]));
}

static void
sse2_test (void)
{
  long long r, ck;
  __v4sf x = { 1.99f, -3.9f, -4.9f, 3.8f };

  /* Run the MMX tests */
  test_cvtps_pi32 ((__m128 *) &x, &r);
  compute_correct_result ((__m128 *) &x, &ck);
  if (ck != r)
    abort ();
}
