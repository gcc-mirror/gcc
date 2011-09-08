/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

void static
avx2_test (void)
{
  union128d s1;
  union256d res;
  double res_ref[4];
  int i, j;
  int fail = 0;

  for (i = 0; i < 10; i++)
    {
      s1.a[0] = i * 3.14;

      res.x = _mm256_broadcastsd_pd (s1.x);

      for (j = 0; j < 4; j++)
	memcpy (res_ref + j, s1.a, 8);

      fail += check_union256d (res, res_ref);
    }

  if (fail != 0)
    abort ();

}
