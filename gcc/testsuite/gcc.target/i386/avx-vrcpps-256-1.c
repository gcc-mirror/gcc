/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mfpmath=sse -mavx" } */

#include "avx-check.h"

static void
avx_test ()
{
    int i;
    union256 u, s1;
    float e[8] = {0.0};

    s1.x = _mm256_set_ps (1.0, 2.0, 13.0, 14.0, 56.89, 73.3, 4.78, 45.64);
    u.x = _mm256_rcp_ps (s1.x);

    for (i = 0; i < 8; i++) {
      __m128 tmp = _mm_load_ss (&s1.a[i]);
      tmp = _mm_rcp_ss (tmp);
      _mm_store_ss (&e[i], tmp);
    }

    if (check_union256 (u, e))
      abort ();
}

