/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

static void
compute_sadbw256 (unsigned char *s1, unsigned char *s2, unsigned short *r)
{
  int i;
  unsigned char tmp[32];

  for (i = 0; i < 32; i++)
    tmp[i] = s1[i] > s2[i] ? s1[i] - s2[i] : s2[i] - s1[i];

  memset (r, 0, 32);

  for (i = 0; i < 8; i++)
    r[0] += tmp[i];

  for (i = 8; i < 16; i++)
    r[4] += tmp[i];

  for (i = 16; i < 24; i++)
    r[8] += tmp[i];

  for (i = 24; i < 32; i++)
    r[12] += tmp[i];
}

void static
avx2_test (void)
{
  union256i_b s1, s2;
  union256i_w res;
  unsigned short res_ref[16];
  int i, j;
  int fail = 0;

  for (i = 0; i < 10; i++)
    {
      for (j = 0; j < 32; j++)
	{
	  s1.a[j] = j * i;
	  s2.a[j] = j + 20;
	}

      res.x = _mm256_sad_epu8 (s1.x, s2.x);;
      compute_sadbw256 (s1.a, s2.a, res_ref);

      fail += check_union256i_w (res, res_ref);
    }

  if (fail != 0)
    abort ();
}
