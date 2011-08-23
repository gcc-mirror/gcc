/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

void static
avx2_test (void)
{
  union128 s1, res;
  float res_ref[4];
  int i, j;
  int fail = 0;

  for (i = 0; i < 10; i++)
    {
      s1.a[0] = i * 3.14;

      res.x = _mm_broadcastss_ps (s1.x);

      for (j = 0; j < 4; j++)
	memcpy (res_ref + j, s1.a, 4);

      fail += check_union128 (res, res_ref);
    }

  if (fail != 0)
    abort ();

}
