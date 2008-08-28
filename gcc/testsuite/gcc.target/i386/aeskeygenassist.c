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
#define IMM8 1

static __m128i src1[NUM];
static __m128i edst[NUM];

static __m128i resdst[NUM];

/* Initialize input/output vectors.  (Currently, there is only one set
   of input/output vectors).  */

static void
init_data (__m128i *s1, __m128i *d)
{
  int i;
  for (i = 0; i < NUM; i++)
    {
      s1[i] = _mm_setr_epi32 (0x16157e2b, 0xa6d2ae28,
			      0x8815f7ab, 0x3c4fcf09);
      d[i] = _mm_setr_epi32 (0x24b5e434, 0x3424b5e5,
			     0xeb848a01, 0x01eb848b);
    }
}

static void
TEST (void)
{
  int i;

  init_data (src1, edst);

  for (i = 0; i < NUM; i += 16)
    {
      resdst[i]  = _mm_aeskeygenassist_si128 (src1[i], IMM8);
      resdst[i + 1] = _mm_aeskeygenassist_si128 (src1[i + 1], IMM8);
      resdst[i + 2] = _mm_aeskeygenassist_si128 (src1[i + 2], IMM8);
      resdst[i + 3] = _mm_aeskeygenassist_si128 (src1[i + 3], IMM8);
      resdst[i + 4] = _mm_aeskeygenassist_si128 (src1[i + 4], IMM8);
      resdst[i + 5] = _mm_aeskeygenassist_si128 (src1[i + 5], IMM8);
      resdst[i + 6] = _mm_aeskeygenassist_si128 (src1[i + 6], IMM8);
      resdst[i + 7] = _mm_aeskeygenassist_si128 (src1[i + 7], IMM8);
      resdst[i + 8] = _mm_aeskeygenassist_si128 (src1[i + 8], IMM8);
      resdst[i + 9] = _mm_aeskeygenassist_si128 (src1[i + 9], IMM8);
      resdst[i + 10] = _mm_aeskeygenassist_si128 (src1[i + 10], IMM8);
      resdst[i + 11] = _mm_aeskeygenassist_si128 (src1[i + 11], IMM8);
      resdst[i + 12] = _mm_aeskeygenassist_si128 (src1[i + 12], IMM8);
      resdst[i + 13] = _mm_aeskeygenassist_si128 (src1[i + 13], IMM8);
      resdst[i + 14] = _mm_aeskeygenassist_si128 (src1[i + 14], IMM8);
      resdst[i + 15] = _mm_aeskeygenassist_si128 (src1[i + 15], IMM8);
    }

  for (i = 0; i < NUM; i++)
    if (memcmp(edst + i, resdst + i, sizeof (__m128i)))
      abort ();
}
