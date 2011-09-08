/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include "avx2-check.h"

void static
avx2_test (void)
{
  union256i_q s1, res;
  int i, j;
  int fail = 0;

  for (i = 0; i < 10; i++)
    {
      for (j = 0; j < 4; j++)
	s1.a[j] = j * i;

      res.x = _mm256_stream_load_si256 (&s1.x);

      fail += check_union256i_q (res, s1.a);
    }

  if (fail != 0)
    abort ();
}
