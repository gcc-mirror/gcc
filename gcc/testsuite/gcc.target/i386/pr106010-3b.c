/* { dg-do run } */
/* { dg-options "-O2 -mavx2 -ftree-vectorize -fvect-cost-model=unlimited -mprefer-vector-width=256" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"
#include <string.h>
#include "pr106010-3a.c"

void
avx2_test (void)
{
  _Complex double* pd_src = (_Complex double*) malloc (32);
  _Complex double* pd_dst = (_Complex double*) malloc (32);
  _Complex double* pd_exp = (_Complex double*) malloc (32);
  _Complex float* ps_src = (_Complex float*) malloc (32);
  _Complex float* ps_dst = (_Complex float*) malloc (32);
  _Complex float* ps_exp = (_Complex float*) malloc (32);
  _Complex long long* epi64_src = (_Complex long long*) malloc (32);
  _Complex long long* epi64_dst = (_Complex long long*) malloc (32);
  _Complex long long* epi64_exp = (_Complex long long*) malloc (32);
  _Complex int* epi32_src = (_Complex int*) malloc (32);
  _Complex int* epi32_dst = (_Complex int*) malloc (32);
  _Complex int* epi32_exp = (_Complex int*) malloc (32);
  _Complex short* epi16_src = (_Complex short*) malloc (32);
  _Complex short* epi16_dst = (_Complex short*) malloc (32);
  _Complex short* epi16_exp = (_Complex short*) malloc (32);
  _Complex char* epi8_src = (_Complex char*) malloc (32);
  _Complex char* epi8_dst = (_Complex char*) malloc (32);
  _Complex char* epi8_exp = (_Complex char*) malloc (32);
  char* p = (char* ) malloc (32);
  char* q = (char* ) malloc (32);

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

  for (int i = 0; i != 16; i++)
    {
      p[i] = i + 16;
      p[i + 16] = i;
    }
  __builtin_memcpy (pd_exp, p, 32);
  __builtin_memcpy (epi64_exp, p, 32);

  for (int i = 0; i != 8; i++)
    {
      p[i] = i + 8;
      p[i + 8] = i;
      p[i + 16] = i + 24;
      p[i + 24] = i + 16;
      q[i] = i + 24;
      q[i + 8] = i + 16;
      q[i + 16] = i + 8;
      q[i + 24] = i;
    }
  __builtin_memcpy (ps_exp, p, 32);
  __builtin_memcpy (epi32_exp, q, 32);


  for (int i = 0; i != 4; i++)
    {
      q[i] = i + 28;
      q[i + 4] = i + 24;
      q[i + 8] = i + 20;
      q[i + 12] = i + 16;
      q[i + 16] = i + 12;
      q[i + 20] = i + 8;
      q[i + 24] = i + 4;
      q[i + 28] = i;
    }
  __builtin_memcpy (epi16_exp, q, 32);

  for (int i = 0; i != 2; i++)
    {
      q[i] = i + 14;
      q[i + 2] = i + 12;
      q[i + 4] = i + 10;
      q[i + 6] = i + 8;
      q[i + 8] = i + 6;
      q[i + 10] = i + 4;
      q[i + 12] = i + 2;
      q[i + 14] = i;
      q[i + 16] = i + 30;
      q[i + 18] = i + 28;
      q[i + 20] = i + 26;
      q[i + 22] = i + 24;
      q[i + 24] = i + 22;
      q[i + 26] = i + 20;
      q[i + 28] = i + 18;
      q[i + 30] = i + 16;
    }
  __builtin_memcpy (epi8_exp, q, 32);

  foo_pd (pd_dst, pd_src);
  foo_ps (ps_dst, ps_src);
  foo_epi64 (epi64_dst, epi64_src);
  foo_epi32 (epi32_dst, epi32_src);
  foo_epi16 (epi16_dst, epi16_src);
  foo_epi8 (epi8_dst, epi8_src);
  if (__builtin_memcmp (pd_dst, pd_exp, 32) != 0)
    __builtin_abort ();
  if (__builtin_memcmp (ps_dst, ps_exp, 32) != 0)
    __builtin_abort ();
  if (__builtin_memcmp (epi64_dst, epi64_exp, 32) != 0)
    __builtin_abort ();
  if (__builtin_memcmp (epi32_dst, epi32_exp, 32) != 0)
    __builtin_abort ();
  if (__builtin_memcmp (epi16_dst, epi16_exp, 32) != 0)
    __builtin_abort ();
  if (__builtin_memcmp (epi8_dst, epi8_exp, 32) != 0)
    __builtin_abort ();

  return;
}
