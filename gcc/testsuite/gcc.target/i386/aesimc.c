/* { dg-do run } */
/* { dg-require-effective-target aes } */
/* { dg-options "-O2 -maes" } */

#ifndef CHECK_H
#define CHECK_H "aes-check.h"
#endif

#ifndef TEST
#define TEST aes_test
#endif

#include CHECK_H

#include <wmmintrin.h>
#include <string.h>

extern void abort (void);

#define NUM 1024

static __m128i src1[NUM];
static __m128i edst[NUM];

static __m128i resdst[NUM];

/* Initialize input/output vectors.  (Currently, there is only one set
   of input/output vectors).   */

static void
init_data (__m128i *s1, __m128i *d)
{
  int i;

  for (i = 0; i < NUM; i++)
    {
      s1[i] = _mm_setr_epi32 (0x5d53475d, 0x63746f72,
			      0x73745665, 0x7b5b5465);
      d[i] = _mm_setr_epi32 (0x81c3b3e5, 0x2b18330a,
			     0x44b109c8, 0x627a6f66);
    }
}

static void
TEST (void)
{
  int i;

  init_data (src1, edst);

  for (i = 0; i < NUM; i += 16)
    {
      resdst[i] = _mm_aesimc_si128 (src1[i]);
      resdst[i + 1] = _mm_aesimc_si128 (src1[i + 1]);
      resdst[i + 2] = _mm_aesimc_si128 (src1[i + 2]);
      resdst[i + 3] = _mm_aesimc_si128 (src1[i + 3]);
      resdst[i + 4] = _mm_aesimc_si128 (src1[i + 4]);
      resdst[i + 5] = _mm_aesimc_si128 (src1[i + 5]);
      resdst[i + 6] = _mm_aesimc_si128 (src1[i + 6]);
      resdst[i + 7] = _mm_aesimc_si128 (src1[i + 7]);
      resdst[i + 8] = _mm_aesimc_si128 (src1[i + 8]);
      resdst[i + 9] = _mm_aesimc_si128 (src1[i + 9]);
      resdst[i + 10] = _mm_aesimc_si128 (src1[i + 10]);
      resdst[i + 11] = _mm_aesimc_si128 (src1[i + 11]);
      resdst[i + 12] = _mm_aesimc_si128 (src1[i + 12]);
      resdst[i + 13] = _mm_aesimc_si128 (src1[i + 13]);
      resdst[i + 14] = _mm_aesimc_si128 (src1[i + 14]);
      resdst[i + 15] = _mm_aesimc_si128 (src1[i + 15]);
    }

  for (i = 0; i < NUM; i++)
    if (memcmp(edst + i, resdst + i, sizeof (__m128i)))
      abort ();
}
