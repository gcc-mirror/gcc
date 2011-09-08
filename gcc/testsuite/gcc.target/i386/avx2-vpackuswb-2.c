/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

static unsigned char
short_to_ubyte (short iVal)
{
  unsigned char sVal;

  if (iVal < 0)
    sVal = 0;
  else if (iVal > 255)
    sVal = 255;
  else
    sVal = iVal;

  return sVal;
}

void static
avx2_test (void)
{
  union256i_w s1, s2;
  union256i_b u;
  char e[32];
  int i;

  s1.x = _mm256_set_epi16 (1, 2, 3, 4, 6500, 20, 30, 90,
			   88, 44, 33, 22, 11, 98, 78, -1000);

  s2.x = _mm256_set_epi16 (88, 44, 33, 22, 11, 98, 76, -650,
			   1, 2, 3, 4, 6500, 20, 30, 90);

  u.x = _mm256_packus_epi16 (s1.x, s2.x);

  for (i = 0; i < 8; i++)
    {
      e[i] = short_to_ubyte (s1.a[i]);
      e[i + 8] = short_to_ubyte (s2.a[i]);
      e[i + 16] = short_to_ubyte (s1.a[i + 8]);
      e[i + 24] = short_to_ubyte (s2.a[i + 8]);
    }

  if (check_union256i_b (u, e))
    abort ();
}
