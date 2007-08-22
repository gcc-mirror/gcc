/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */

#include "sse4_1-check.h"

#include <smmintrin.h>

static void
sse4_1_test (void)
{
  union
    {
      __m128i x;
      unsigned int i[4];
    } val[4];
  int correct_zeros[4];
  int correct_ones[4];
  int correct_mixed[4];
  int zeros[4];
  int ones[4];
  int mixed[4];
  int i;
  __m128i v;

  val[0].i[0] = 0x11111111;
  val[0].i[1] = 0x00000000;
  val[0].i[2] = 0x00000000;
  val[0].i[3] = 0x11111111;
  correct_zeros[0] = 0;
  correct_ones[0] = 0;
  correct_mixed[0] = 1;
    
  val[1].i[0] = 0x00000000;
  val[1].i[1] = 0x11111111;
  val[1].i[2] = 0x11111111;
  val[1].i[3] = 0x00000000;
  correct_zeros[1] = 0;
  correct_ones[1] = 0;
  correct_mixed[1] = 1;

  val[2].i[0] = 0;
  val[2].i[1] = 0;
  val[2].i[2] = 0;
  val[2].i[3] = 0;
  correct_zeros[2] = 1;
  correct_ones[2] = 0;
  correct_mixed[2] = 0;

  val[3].i[0] = 0xffffffff;
  val[3].i[1] = 0xffffffff;
  val[3].i[2] = 0xffffffff;
  val[3].i[3] = 0xffffffff;
  correct_zeros[3] = 0;
  correct_ones[3] = 1;
  correct_mixed[3] = 0;

  for (i=0; i < 4; i++)
    zeros[i] = _mm_test_all_zeros (val[i].x, val[i].x);

  for( i=0; i < 4; i++ )
    ones[i] = _mm_test_all_ones (val[i].x);

  v = _mm_cmpeq_epi32 (val[0].x, val[0].x);
  for( i=0; i < 4; i++ )
    mixed[i] = _mm_test_mix_ones_zeros (val[i].x, v);

  for( i=0; i < 4; i++ )
    {
      if (zeros[i] != correct_zeros[i])
	abort ();
      if (ones[i] != correct_ones[i])
	abort ();
      if (mixed[i] != correct_mixed[i])
	abort ();
    }
}
