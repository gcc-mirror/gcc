/* { dg-do run } */
/* { dg-options "-O2 -mavx -ftree-vectorize -fvect-cost-model=unlimited -mprefer-vector-width=256" } */
/* { dg-require-effective-target avx } */

#include "avx-check.h"
#include <string.h>
#include "pr106010-1a.c"

void
avx_test (void)
{
  _Complex double* pd_src = (_Complex double*) malloc (2 * N * sizeof (double));
  _Complex double* pd_dst = (_Complex double*) malloc (2 * N * sizeof (double));
  _Complex float* ps_src = (_Complex float*) malloc (2 * N * sizeof (float));
  _Complex float* ps_dst = (_Complex float*) malloc (2 * N * sizeof (float));
  _Complex long long* epi64_src = (_Complex long long*) malloc (2 * N * sizeof (long long));
  _Complex long long* epi64_dst = (_Complex long long*) malloc (2 * N * sizeof (long long));
  _Complex int* epi32_src = (_Complex int*) malloc (2 * N * sizeof (int));
  _Complex int* epi32_dst = (_Complex int*) malloc (2 * N * sizeof (int));
  _Complex short* epi16_src = (_Complex short*) malloc (2 * N * sizeof (short));
  _Complex short* epi16_dst = (_Complex short*) malloc (2 * N * sizeof (short));
  _Complex char* epi8_src = (_Complex char*) malloc (2 * N * sizeof (char));
  _Complex char* epi8_dst = (_Complex char*) malloc (2 * N * sizeof (char));
  char* p_init = (char*) malloc (2 * N * sizeof (double));

  __builtin_memset (pd_dst, 0, 2 * N * sizeof (double));
  __builtin_memset (ps_dst, 0, 2 * N * sizeof (float));
  __builtin_memset (epi64_dst, 0, 2 * N * sizeof (long long));
  __builtin_memset (epi32_dst, 0, 2 * N * sizeof (int));
  __builtin_memset (epi16_dst, 0, 2 * N * sizeof (short));
  __builtin_memset (epi8_dst, 0, 2 * N * sizeof (char));

  for (int i = 0; i != 2 * N * sizeof (double); i++)
    p_init[i] = i;

  memcpy (pd_src, p_init, 2 * N * sizeof (double));
  memcpy (ps_src, p_init, 2 * N * sizeof (float));
  memcpy (epi64_src, p_init, 2 * N * sizeof (long long));
  memcpy (epi32_src, p_init, 2 * N * sizeof (int));
  memcpy (epi16_src, p_init, 2 * N * sizeof (short));
  memcpy (epi8_src, p_init, 2 * N * sizeof (char));

  foo_pd (pd_dst, pd_src);
  foo_ps (ps_dst, ps_src);
  foo_epi64 (epi64_dst, epi64_src);
  foo_epi32 (epi32_dst, epi32_src);
  foo_epi16 (epi16_dst, epi16_src);
  foo_epi8 (epi8_dst, epi8_src);
  if (__builtin_memcmp (pd_dst, pd_src, N * 2 * sizeof (double)) != 0)
    __builtin_abort ();
  if (__builtin_memcmp (ps_dst, ps_src, N * 2 * sizeof (float)) != 0)
    __builtin_abort ();
  if (__builtin_memcmp (epi64_dst, epi64_src, N * 2 * sizeof (long long)) != 0)
    __builtin_abort ();
  if (__builtin_memcmp (epi32_dst, epi32_src, N * 2 * sizeof (int)) != 0)
    __builtin_abort ();
  if (__builtin_memcmp (epi16_dst, epi16_src, N * 2 * sizeof (short)) != 0)
    __builtin_abort ();
  if (__builtin_memcmp (epi8_dst, epi8_src, N * 2 * sizeof (char)) != 0)
    __builtin_abort ();

  return;
}
