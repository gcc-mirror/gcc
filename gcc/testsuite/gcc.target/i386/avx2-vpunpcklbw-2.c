/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

#define N 0x5

static void
compute_punpcklbw256 (char *s1, char *s2, char *r)
{
  int i;

  for (i = 0; i < 8; i++)
    {
      r[2 * i] = s1[i];
      r[2 * i + 1] = s2[i];

      r[2 * i + 16] = s1[i + 16];
      r[2 * i + 16 + 1] = s2[i + 16];
    }
}

void static
avx2_test (void)
{
  union256i_b s1, s2, res;
  char res_ref[32];
  int i, j;
  int fail = 0;

  for (i = 0; i < 10; i++)
    {
      for (j = 0; j < 32; j++)
	{
	  s1.a[j] = j * i;
	  s2.a[j] = j + 20;
	}

      res.x = _mm256_unpacklo_epi8 (s1.x, s2.x);

      compute_punpcklbw256 (s1.a, s2.a, res_ref);

      fail += check_union256i_b (res, res_ref);
    }

  if (fail != 0)
    abort ();
}
