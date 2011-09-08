/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

void static
avx2_test (void)
{
  union128i_q s1;
  union256i_q res;
  long long int res_ref[4];
  int i, j;
  int fail = 0;

  for (i = 0; i < 10; i++)
    {
      for (j = 0; j < 2; j++)
	s1.a[j] = j * i;

      res.x = _mm_broadcastsi128_si256 (s1.x);

      memcpy (res_ref, s1.a, 16);
      memcpy (res_ref + 2, s1.a, 16);

      fail += check_union256i_q (res, res_ref);
    }

  if (fail != 0)
    abort ();

}
