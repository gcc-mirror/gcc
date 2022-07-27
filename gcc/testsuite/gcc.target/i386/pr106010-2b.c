/* { dg-do run } */
/* { dg-options "-O2 -mavx -ftree-vectorize -fvect-cost-model=unlimited -mprefer-vector-width=256" } */
/* { dg-require-effective-target avx } */

#include "avx-check.h"
#include <string.h>
#include "pr106010-2a.c"

void
avx_test (void)
{
  _Complex double* pd_src = (_Complex double*) malloc (32);
  _Complex double* pd_dst = (_Complex double*) malloc (32);
  _Complex float* ps_src = (_Complex float*) malloc (32);
  _Complex float* ps_dst = (_Complex float*) malloc (32);
  _Complex long long* epi64_src = (_Complex long long*) malloc (32);
  _Complex long long* epi64_dst = (_Complex long long*) malloc (32);
  _Complex int* epi32_src = (_Complex int*) malloc (32);
  _Complex int* epi32_dst = (_Complex int*) malloc (32);
  _Complex short* epi16_src = (_Complex short*) malloc (32);
  _Complex short* epi16_dst = (_Complex short*) malloc (32);
  _Complex char* epi8_src = (_Complex char*) malloc (32);
  _Complex char* epi8_dst = (_Complex char*) malloc (32);
  char* p = (char* ) malloc (32);

  __builtin_memset (pd_dst, 0, 32);
  __builtin_memset (ps_dst, 0, 32);
  __builtin_memset (epi64_dst, 0, 32);
  __builtin_memset (epi32_dst, 0, 32);
  __builtin_memset (epi16_dst, 0, 32);
  __builtin_memset (epi8_dst, 0, 32);

  for (int i = 0; i != 32; i++)
    p[i] = i;
  __builtin_memcpy (pd_src, p, 32);
  __builtin_memcpy (ps_src, p, 32);
  __builtin_memcpy (epi64_src, p, 32);
  __builtin_memcpy (epi32_src, p, 32);
  __builtin_memcpy (epi16_src, p, 32);
  __builtin_memcpy (epi8_src, p, 32);

  foo_pd (pd_dst, pd_src);
  foo_ps (ps_dst, ps_src);
  foo_epi64 (epi64_dst, epi64_src);
  foo_epi32 (epi32_dst, epi32_src);
  foo_epi16 (epi16_dst, epi16_src);
  foo_epi8 (epi8_dst, epi8_src);
  if (__builtin_memcmp (pd_dst, pd_src, 32) != 0)
    __builtin_abort ();
  if (__builtin_memcmp (ps_dst, ps_src, 32) != 0)
    __builtin_abort ();
  if (__builtin_memcmp (epi64_dst, epi64_src, 32) != 0)
    __builtin_abort ();
  if (__builtin_memcmp (epi32_dst, epi32_src, 32) != 0)
    __builtin_abort ();
  if (__builtin_memcmp (epi16_dst, epi16_src, 32) != 0)
    __builtin_abort ();
  if (__builtin_memcmp (epi16_dst, epi16_src, 32) != 0)
    __builtin_abort ();

  return;
}
