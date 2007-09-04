/* { dg-do run } */
/* { dg-require-effective-target sse4a } */
/* { dg-options "-O2 -msse4a" } */

#include "sse4a-check.h"

#include <ammintrin.h>

static void 
sse4a_test_movntss (float *out, float *in)
{
  __m128 in_v4sf = _mm_load_ss (in);
  _mm_stream_ss (out, in_v4sf);
}

static int 
chk_ss (float *v1, float *v2)
{
  int n_fails = 0;
  if (v1[0] != v2[0])
    n_fails += 1;
  return n_fails;
}

float vals[10] =
  {
    100.0,  200.0, 300.0, 400.0, 5.0, 
    -1.0, .345, -21.5, 9.32,  8.41
  };

static void
sse4a_test (void)
{
  int i;
  int fail = 0;
  float *out;

  out = (float *) malloc (sizeof (float));
  for (i = 0; i < 10; i += 1)
    {
      sse4a_test_movntss (out, &vals[i]);
      
      fail += chk_ss (out, &vals[i]);
    }

  if (fail != 0)
    abort ();
}
