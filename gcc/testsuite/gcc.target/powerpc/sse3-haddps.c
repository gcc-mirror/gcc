/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse3-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse3_test_haddps_1
#endif

#define NO_WARN_X86_INTRINSICS 1
#include <pmmintrin.h>

static void
sse3_test_haddps (float *i1, float *i2, float *r)
{
  __m128 t1 = _mm_loadu_ps (i1);
  __m128 t2 = _mm_loadu_ps (i2);

  t1 = _mm_hadd_ps (t1, t2);

  _mm_storeu_ps (r, t1);
}

static void
sse3_test_haddps_subsume (float *i1, float *i2, float *r)
{
  __m128 t1 = _mm_load_ps (i1);
  __m128 t2 = _mm_load_ps (i2);

  t1 = _mm_hadd_ps (t1, t2);

  _mm_storeu_ps (r, t1);
}

static int
chk_ps(float *v1, float *v2)
{
  int i;
  int n_fails = 0;

  for (i = 0; i < 4; i++)
    if (v1[i] != v2[i])
      n_fails += 1;

  return n_fails;
}

static float p1[4] __attribute__ ((aligned(16)));
static float p2[4] __attribute__ ((aligned(16)));
static float p3[4];
static float ck[4];

static float vals[] =
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

//static
void
TEST ()
{
  int i;
  int fail = 0;

  for (i = 0; i < sizeof (vals) / sizeof (vals[0]); i += 8)
    {
      p1[0] = vals[i+0];
      p1[1] = vals[i+1];
      p1[2] = vals[i+2];
      p1[3] = vals[i+3];

      p2[0] = vals[i+4];
      p2[1] = vals[i+5];
      p2[2] = vals[i+6];
      p2[3] = vals[i+7];

      ck[0] = p1[0] + p1[1];
      ck[1] = p1[2] + p1[3];
      ck[2] = p2[0] + p2[1];
      ck[3] = p2[2] + p2[3];

      sse3_test_haddps (p1, p2, p3);

      fail += chk_ps (ck, p3);

      sse3_test_haddps_subsume (p1, p2, p3);

      fail += chk_ps (ck, p3);
    }

  if (fail != 0)
    abort ();
}
