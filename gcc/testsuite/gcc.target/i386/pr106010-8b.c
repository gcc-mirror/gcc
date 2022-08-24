/* { dg-do run } */
/* { dg-options "-O2 -mavx -ftree-vectorize -fvect-cost-model=unlimited -mprefer-vector-width=256" } */
/* { dg-require-effective-target avx } */

#include "avx-check.h"
#include <string.h>
#include "pr106010-8a.c"

void
avx_test (void)
{
  _Complex double pd_src = 1.0 + 2.0i;
  _Complex double* pd_dst = (_Complex double*) malloc (2 * N * sizeof (double));
  _Complex float ps_src = 1.0 + 2.0i;
  _Complex float* ps_dst = (_Complex float*) malloc (2 * N * sizeof (float));
  _Complex long long epi64_src = 1 + 2i;;
  _Complex long long* epi64_dst = (_Complex long long*) malloc (2 * N * sizeof (long long));
  _Complex int epi32_src = 1 + 2i;
  _Complex int* epi32_dst = (_Complex int*) malloc (2 * N * sizeof (int));
  _Complex short epi16_src = 1 + 2i;
  _Complex short* epi16_dst = (_Complex short*) malloc (2 * N * sizeof (short));
  _Complex char epi8_src = 1 + 2i;
  _Complex char* epi8_dst = (_Complex char*) malloc (2 * N * sizeof (char));

  __builtin_memset (pd_dst, 0, 2 * N * sizeof (double));
  __builtin_memset (ps_dst, 0, 2 * N * sizeof (float));
  __builtin_memset (epi64_dst, 0, 2 * N * sizeof (long long));
  __builtin_memset (epi32_dst, 0, 2 * N * sizeof (int));
  __builtin_memset (epi16_dst, 0, 2 * N * sizeof (short));
  __builtin_memset (epi8_dst, 0, 2 * N * sizeof (char));

  foo_pd (pd_dst);
  foo_ps (ps_dst);
  foo_epi64 (epi64_dst);
  foo_epi32 (epi32_dst);
  foo_epi16 (epi16_dst);
  foo_epi8 (epi8_dst);
  for (int i = 0 ; i != N; i++)
    {
      if (pd_dst[i] != pd_src)
	__builtin_abort ();
      if (ps_dst[i] != ps_src)
	__builtin_abort ();
      if (epi64_dst[i] != epi64_src)
	__builtin_abort ();
      if (epi32_dst[i] != epi32_src)
	__builtin_abort ();
      if (epi16_dst[i] != epi16_src)
	__builtin_abort ();
      if (epi8_dst[i] != epi8_src)
	__builtin_abort ();
    }
}
