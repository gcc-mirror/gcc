/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

static short
int_to_short (int iVal)
{
  short sVal;

  if (iVal < -32768)
    sVal = -32768;
  else if (iVal > 32767)
    sVal = 32767;
  else
    sVal = iVal;

  return sVal;
}

void static
avx2_test (void)
{
  union256i_d s1, s2;
  union256i_w u;
  short e[16];
  int i;

  s1.x = _mm256_set_epi32 (1, 2, 3, 4, 65000, 20, 30, 90);

  s2.x = _mm256_set_epi32 (88, 44, 33, 22, 11, 98, 76, -65000);

  u.x = _mm256_packs_epi32 (s1.x, s2.x);

  for (i = 0; i < 4; i++)
    {
      e[i] = int_to_short (s1.a[i]);
      e[i + 4] = int_to_short (s2.a[i]);
      e[i + 8] = int_to_short (s1.a[i + 4]);
      e[i + 12] = int_to_short (s2.a[i + 4]);
    }

  if (check_union256i_w (u, e))
    abort ();
}
