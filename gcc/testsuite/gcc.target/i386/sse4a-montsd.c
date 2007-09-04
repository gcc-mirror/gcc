/* { dg-do run } */
/* { dg-require-effective-target sse4a } */
/* { dg-options "-O2 -msse4a" } */

#include "sse4a-check.h"

#include <ammintrin.h>

static void 
sse4a_test_movntsd (double *out, double *in)
{
  __m128d in_v2df = _mm_load_sd (in);
  _mm_stream_sd (out, in_v2df);
}

static int 
chk_sd (double *v1, double *v2)
{
  int n_fails = 0;
  if (v1[0] != v2[0])
    n_fails += 1;
  return n_fails;
}

double vals[10] =
  {
    100.0,  200.0, 300.0, 400.0, 5.0, 
    -1.0, .345, -21.5, 9.32,  8.41
  };

static void
sse4a_test (void)
{
  int i;
  int fail = 0;
  double *out;

  out = (double *) malloc (sizeof (double));
  for (i = 0; i < 10; i += 1)
    {
      sse4a_test_movntsd (out, &vals[i]);
      
      fail += chk_sd (out, &vals[i]);
    }

  if (fail != 0)
    abort ();
}
