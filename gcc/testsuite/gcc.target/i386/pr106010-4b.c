/* { dg-do run } */
/* { dg-options "-O2 -mavx -ftree-vectorize -fvect-cost-model=unlimited -mprefer-vector-width=256" } */
/* { dg-require-effective-target avx } */

#include "avx-check.h"
#include <string.h>
#include "pr106010-4a.c"

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

  foo_pd (pd_dst, pd_src[0], pd_src[1]);
  foo_ps (ps_dst, ps_src[0], ps_src[1], ps_src[2], ps_src[3]);
  foo_epi64 (epi64_dst, epi64_src[0], epi64_src[1]);
  foo_epi32 (epi32_dst, epi32_src[0], epi32_src[1], epi32_src[2], epi32_src[3]);
  foo_epi16 (epi16_dst, epi16_src[0], epi16_src[1], epi16_src[2], epi16_src[3],
	     epi16_src[4], epi16_src[5], epi16_src[6], epi16_src[7]);
  foo_epi8 (epi8_dst, epi8_src[0], epi8_src[1], epi8_src[2], epi8_src[3],
	    epi8_src[4], epi8_src[5], epi8_src[6], epi8_src[7],
	    epi8_src[8], epi8_src[9], epi8_src[10], epi8_src[11],
	    epi8_src[12], epi8_src[13], epi8_src[14], epi8_src[15]);

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
  if (__builtin_memcmp (epi8_dst, epi8_src, 32) != 0)
    __builtin_abort ();

  return;
}
