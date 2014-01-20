/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

void static
avx512f_test (void)
{
  union512i_q s, res;

  s.x = _mm512_set_epi64 (39578, -429496, 7856, 0, 85632, -1234, 47563, -1);
  res.x = _mm512_stream_load_si512 (&s.x);

  if (check_union512i_q (s, res.a))
    abort ();
}
