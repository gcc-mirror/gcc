/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

void static
avx512f_test (void)
{
  union512d s;
  double res[8] __attribute__((aligned (64)));

  s.x = _mm512_set_pd (-39578.467285, 4294967295.1, -7856.342941, 0,
		       85632.783567, 1234.9999, 47563.234215, -1.07);
  _mm512_stream_pd (res, s.x);

  if (check_union512d (s, res))
    abort ();
}
