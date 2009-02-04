/* { dg-do run } */
/* { dg-require-effective-target pclmul } */
/* { dg-options "-O2 -mpclmul" } */

#ifndef CHECK_H
#define CHECK_H "pclmul-check.h"
#endif

#ifndef TEST
#define TEST pclmul_test
#endif

#include CHECK_H

#include <wmmintrin.h>
#include <string.h>

extern void abort (void);

#define NUM 1024

static __m128i s1[NUM];
static __m128i s2[NUM];
/* We need this array to generate mem form of inst */
static __m128i s2m[NUM];

static __m128i e_00[NUM];
static __m128i e_01[NUM];
static __m128i e_10[NUM];
static __m128i e_11[NUM];

static __m128i d_00[NUM];
static __m128i d_01[NUM];
static __m128i d_10[NUM];
static __m128i d_11[NUM];

/* Initialize input/output vectors.  (Currently, there is only one set
   of input/output vectors).  */
static void
init_data (__m128i *ls1,   __m128i *ls2, __m128i *le_00, __m128i *le_01,
	   __m128i *le_10, __m128i *le_11)
{
  int i;

  for (i = 0; i < NUM; i++)
    {
      ls1[i] = _mm_set_epi32 (0x7B5B5465, 0x73745665,
			      0x63746F72, 0x5D53475D);
      ls2[i] = _mm_set_epi32 (0x48692853, 0x68617929,
			      0x5B477565, 0x726F6E5D);
      s2m[i] = _mm_set_epi32 (0x48692853, 0x68617929,
			      0x5B477565, 0x726F6E5D);
      le_00[i] = _mm_set_epi32 (0x1D4D84C8, 0x5C3440C0,
				0x929633D5, 0xD36F0451);
      le_01[i] = _mm_set_epi32 (0x1A2BF6DB, 0x3A30862F,
				0xBABF262D, 0xF4B7D5C9);
      le_10[i] = _mm_set_epi32 (0x1BD17C8D, 0x556AB5A1,
				0x7FA540AC, 0x2A281315);
      le_11[i] = _mm_set_epi32 (0x1D1E1F2C, 0x592E7C45,
				0xD66EE03E, 0x410FD4ED);
    }
}

static void
TEST (void)
{
  int i;

  init_data (s1, s2, e_00, e_01, e_10, e_11);

  for (i = 0; i < NUM; i += 2)
    {
      d_00[i] = _mm_clmulepi64_si128 (s1[i], s2m[i], 0x00);
      d_01[i] = _mm_clmulepi64_si128 (s1[i], s2[i], 0x01);
      d_10[i] = _mm_clmulepi64_si128 (s1[i], s2[i], 0x10);
      d_11[i] = _mm_clmulepi64_si128 (s1[i], s2[i], 0x11);

      d_11[i + 1] = _mm_clmulepi64_si128 (s1[i + 1], s2[i + 1], 0x11);
      d_00[i + 1] = _mm_clmulepi64_si128 (s1[i + 1], s2[i + 1], 0x00);
      d_10[i + 1] = _mm_clmulepi64_si128 (s1[i + 1], s2m[i + 1], 0x10);
      d_01[i + 1] = _mm_clmulepi64_si128 (s1[i + 1], s2[i + 1], 0x01);
    }

  for (i = 0; i < NUM; i++)
    {
      if (memcmp (d_00 + i, e_00 + i, sizeof (__m128i)))
	abort ();
      if (memcmp (d_01 + i, e_01 + i, sizeof (__m128i)))
	abort ();
      if (memcmp (d_10 + i, e_10 + i, sizeof (__m128i)))
	abort ();
      if (memcmp(d_11 + i, e_11 + i, sizeof (__m128i)))
	abort ();
    }
}
