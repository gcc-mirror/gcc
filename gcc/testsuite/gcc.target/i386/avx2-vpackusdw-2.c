/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

static unsigned short
int_to_ushort (int iVal)
{
  unsigned short sVal;

  if (iVal < 0)
    sVal = 0;
  else if (iVal > 65536)
    sVal = 65535;
  else
    sVal = iVal;

  return sVal;
}

void static
avx2_test (void)
{
  union256i_d s1, s2;
  union256i_w u;
  unsigned short e[16];
  int i;

  s1.x = _mm256_set_epi32 (1, 2, 3, 4, -65000, 20, 30, 90);

  s2.x = _mm256_set_epi32 (88, 44, 33, 22, 11, 98, 76, 120000);

  u.x = _mm256_packus_epi32 (s1.x, s2.x);

  for (i = 0; i < 4; i++)
    {
      e[i] = int_to_ushort (s1.a[i]);
      e[i + 4] = int_to_ushort (s2.a[i]);
      e[i + 8] = int_to_ushort (s1.a[i + 4]);
      e[i + 12] = int_to_ushort (s2.a[i + 4]);
    }

  if (check_union256i_w (u, e))
    abort ();
}
