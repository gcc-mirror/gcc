/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

void static
avx2_test (void)
{
  union256i_q s1;
  union128i_q res;
  long long int res_ref[2];
  int j;

  for (j = 0; j < 4; j++)
    s1.a[j] = j * j;

  res.x = _mm256_extracti128_si256 (s1.x, 0);

  memset (res_ref, 0, 16);
  memcpy (res_ref, s1.a, 16);

  if (check_union128i_q (res, res_ref))
    abort ();

  res.x = _mm256_extracti128_si256 (s1.x, 1);

  memset (res_ref, 0, 16);
  memcpy (res_ref, s1.a + 2, 16);

  if (check_union128i_q (res, res_ref))
    abort ();
}
