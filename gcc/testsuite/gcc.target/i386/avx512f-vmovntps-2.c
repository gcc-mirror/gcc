/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

void static
avx512f_test (void)
{
  union512 s;
  float res[16];

  s.x = _mm512_set_ps (-39578.467285, 4294967295.1, -7856.342941, 0,
		       85632.783567, 1234.9999, 47563.234215, -1.07,
		       3453.65743, -1234.9999, 67.234, -1,
		       0.336624, 34534543, 4345.234234, -1.07234234);

  _mm512_stream_ps (res, s.x);

  if (check_union512 (s, res))
    abort ();
}
