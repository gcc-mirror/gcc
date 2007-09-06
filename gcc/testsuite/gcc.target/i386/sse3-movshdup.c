/* { dg-do run } */
/* { dg-options "-O2 -msse3 -mfpmath=sse" } */

#include "sse3-check.h"

#include <pmmintrin.h>

static void
sse3_test_movshdup_reg (float *i1, float *r)
{
  __m128 t1 = _mm_loadu_ps (i1);
  __m128 t2 = _mm_movehdup_ps (t1);

  _mm_storeu_ps (r, t2);
}

static void
sse3_test_movshdup_reg_subsume (float *i1, float *r)
{
  __m128 t1 = _mm_load_ps (i1);
  __m128 t2 = _mm_movehdup_ps (t1);

  _mm_storeu_ps (r, t2);
}

static int
chk_ps (float *v1, float *v2)
{
  int i;
  int n_fails = 0;

  for (i = 0; i < 4; i++)
    if (v1[i] != v2[i])
      n_fails += 1;

  return n_fails;
}

static float p1[4] __attribute__ ((aligned(16)));
static float p2[4];
static float ck[4];

static float vals[80] =
  {
    100.0,  200.0, 300.0, 400.0, 5.0, -1.0, .345, -21.5,
    1100.0, 0.235, 321.3, 53.40, 0.3, 10.0, 42.0, 32.52,
    32.6,   123.3, 1.234, 2.156, 0.1, 3.25, 4.75, 32.44,
    12.16,  52.34, 64.12, 71.13, -.1, 2.30, 5.12, 3.785,
    541.3,  321.4, 231.4, 531.4, 71., 321., 231., -531.,
    23.45,  23.45, 23.45, 23.45, 23.45, 23.45, 23.45, 23.45,
    23.45,  -1.43, -6.74, 6.345, -20.1, -20.1, -40.1, -40.1,
    1.234,  2.345, 3.456, 4.567, 5.678, 6.789, 7.891, 8.912,
    -9.32,  -8.41, -7.50, -6.59, -5.68, -4.77, -3.86, -2.95,
    9.32,  8.41, 7.50, 6.59, -5.68, -4.77, -3.86, -2.95
  };

static void
sse3_test (void)
{
  int i;
  int fail = 0;

  for (i = 0; i < 80; i += 2)
    {
      p1[0] = 0.0;
      p1[1] = vals[i+0];
      p1[2] = 1.0;
      p1[3] = vals[i+1];

      ck[0] = p1[1];
      ck[1] = p1[1];
      ck[2] = p1[3];
      ck[3] = p1[3];

      sse3_test_movshdup_reg (p1, p2);

      fail += chk_ps (ck, p2);

      sse3_test_movshdup_reg_subsume (p1, p2);

      fail += chk_ps (ck, p2);
    }

  if (fail != 0)
    abort ();
}
