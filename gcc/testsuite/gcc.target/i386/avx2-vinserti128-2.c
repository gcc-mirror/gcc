/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

void static
avx2_test (void)
{
  union256i_q s1, res;
  union128i_q s2;
  long long int res_ref[4];
  int j;

  for (j = 0; j < 4; j++)
    s1.a[j] = j * j;

  for (j = 0; j < 2; j++)
    s2.a[j] = j * j * j;

  res.x = _mm256_inserti128_si256 (s1.x, s2.x, 0);

  memcpy (res_ref, s1.a, 32);
  memcpy (res_ref, s2.a, 16);

  if (check_union256i_q (res, res_ref))
    abort ();

  res.x = _mm256_inserti128_si256 (s1.x, s2.x, 1);

  memcpy (res_ref, s1.a, 32);
  memcpy (res_ref + 2, s2.a, 16);

  if (check_union256i_q (res, res_ref))
    abort ();
}
