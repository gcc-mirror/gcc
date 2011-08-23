/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

#define N 0x5

static void
compute_psrldq256 (char *s1, char *r)
{
  int i;

  memset (r, 0, 32);

  for (i = 0; i < 16 - N; i++)
    r[i] = s1[i + N];

  for (i = 0; i < 16 - N; i++)
    r[i + 16] = s1[i + N + 16];
}


void static
avx2_test (void)
{
  union256i_b s1, res;
  char res_ref[32];
  int i, j;
  int fail = 0;

  for (i = 0; i < 10; i++)
    {
      for (j = 0; j < 32; j++)
	s1.a[j] = j * i;

      res.x = _mm256_srli_si256 (s1.x, N);

      compute_psrldq256 (s1.a, res_ref);

      fail += check_union256i_b (res, res_ref);
    }

  if (fail != 0)
    abort ();
}
