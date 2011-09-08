/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

static void
compute_pshufb256 (char *s1, char *s2, char *r)
{
  int i;
  char select;

  for (i = 0; i < 16; i++)
    {
      select = s2[i];
      if (select & 0x80)
	r[i] = 0;
      else
	r[i] = s1[select & 0xf];

      select = s2[i + 16];
      if (select & 0x80)
	r[i + 16] = 0;
      else
	r[i + 16] = s1[16 + (select & 0xf)];
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

      res.x = _mm256_shuffle_epi8 (s1.x, s2.x);
      compute_pshufb256 (s1.a, s2.a, res_ref);

      fail += check_union256i_b (res, res_ref);
    }

  if (fail != 0)
    abort ();
}
