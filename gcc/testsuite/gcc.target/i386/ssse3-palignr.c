/* { dg-do run } */
/* { dg-require-effective-target ssse3 } */
/* { dg-options "-O2 -fno-strict-aliasing -mssse3" } */

#ifndef CHECK_H
#define CHECK_H "ssse3-check.h"
#endif

#ifndef TEST
#define TEST ssse3_test
#endif

#include CHECK_H

#include "ssse3-vals.h"

#include <tmmintrin.h>
#include <string.h>

/* Test the 64-bit form */
static void
ssse3_test_palignr (int *i1, int *i2, unsigned int imm, int *r)
{
  __m64 t1 = *(__m64 *) i1;
  __m64 t2 = *(__m64 *) i2;

  switch (imm)
    {
    case 0:
      *(__m64 *) r = _mm_alignr_pi8 (t1, t2, 0);
      break;
    case 1:
      *(__m64 *) r = _mm_alignr_pi8 (t1, t2, 1);
      break;
    case 2:
      *(__m64 *) r = _mm_alignr_pi8 (t1, t2, 2);
      break;
    case 3:
      *(__m64 *) r = _mm_alignr_pi8 (t1, t2, 3);
      break;
    case 4:
      *(__m64 *) r = _mm_alignr_pi8 (t1, t2, 4);
      break;
    case 5:
      *(__m64 *) r = _mm_alignr_pi8 (t1, t2, 5);
      break;
    case 6:
      *(__m64 *) r = _mm_alignr_pi8 (t1, t2, 6);
      break;
    case 7:
      *(__m64 *) r = _mm_alignr_pi8 (t1, t2, 7);
      break;
    case 8:
      *(__m64 *) r = _mm_alignr_pi8 (t1, t2, 8);
      break;
    case 9:
      *(__m64 *) r = _mm_alignr_pi8 (t1, t2, 9);
      break;
    case 10:
      *(__m64 *) r = _mm_alignr_pi8 (t1, t2, 10);
      break;
    case 11:
      *(__m64 *) r = _mm_alignr_pi8 (t1, t2, 11);
      break;
    case 12:
      *(__m64 *) r = _mm_alignr_pi8 (t1, t2, 12);
      break;
    case 13:
      *(__m64 *) r = _mm_alignr_pi8 (t1, t2, 13);
      break;
    case 14:
      *(__m64 *) r = _mm_alignr_pi8 (t1, t2, 14);
      break;
    case 15:
      *(__m64 *) r = _mm_alignr_pi8 (t1, t2, 15);
      break;
    default:
      *(__m64 *) r = _mm_alignr_pi8 (t1, t2, 16);
      break;
    }

   _mm_empty();
}

/* Test the 128-bit form */
static void
ssse3_test_palignr128 (int *i1, int *i2, unsigned int imm, int *r)
{
  /* Assumes incoming pointers are 16-byte aligned */
  __m128i t1 = *(__m128i *) i1;
  __m128i t2 = *(__m128i *) i2;

  switch (imm)
    {
    case 0:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 0);
      break;
    case 1:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 1);
      break;
    case 2:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 2);
      break;
    case 3:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 3);
      break;
    case 4:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 4);
      break;
    case 5:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 5);
      break;
    case 6:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 6);
      break;
    case 7:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 7);
      break;
    case 8:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 8);
      break;
    case 9:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 9);
      break;
    case 10:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 10);
      break;
    case 11:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 11);
      break;
    case 12:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 12);
      break;
    case 13:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 13);
      break;
    case 14:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 14);
      break;
    case 15:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 15);
      break;
    case 16:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 16);
      break;
    case 17:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 17);
      break;
    case 18:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 18);
      break;
    case 19:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 19);
      break;
    case 20:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 20);
      break;
    case 21:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 21);
      break;
    case 22:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 22);
      break;
    case 23:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 23);
      break;
    case 24:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 24);
      break;
    case 25:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 25);
      break;
    case 26:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 26);
      break;
    case 27:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 27);
      break;
    case 28:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 28);
      break;
    case 29:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 29);
      break;
    case 30:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 30);
      break;
    case 31:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 31);
      break;
    default:
      *(__m128i *) r = _mm_alignr_epi8 (t1, t2, 32);
      break;
    }
}

/* Routine to manually compute the results */
static void
compute_correct_result_128 (int *i1, int *i2, unsigned int imm, int *r)
{
  char buf [32];
  char *bout = (char *) r;
  int i;

  memcpy (&buf[0], i2, 16);
  memcpy (&buf[16], i1, 16);

  for (i = 0; i < 16; i++)
    if (imm >= 32 || imm + i >= 32)
      bout[i] = 0;
    else
      bout[i] = buf[imm + i];
}

static void
compute_correct_result_64 (int *i1, int *i2, unsigned int imm, int *r)
{
  char buf [16];
  char *bout = (char *)r;
  int i;

  /* Handle the first half */
  memcpy (&buf[0], i2, 8);
  memcpy (&buf[8], i1, 8);

  for (i = 0; i < 8; i++)
    if (imm >= 16 || imm + i >= 16)
      bout[i] = 0;
    else
      bout[i] = buf[imm + i];

  /* Handle the second half */
  memcpy (&buf[0], &i2[2], 8);
  memcpy (&buf[8], &i1[2], 8);

  for (i = 0; i < 8; i++)
    if (imm >= 16 || imm + i >= 16)
      bout[i + 8] = 0;
    else
      bout[i + 8] = buf[imm + i];
}

static void
TEST (void)
{
  int i;
  int r [4] __attribute__ ((aligned(16)));
  int ck [4];
  unsigned int imm;
  int fail = 0;

  for (i = 0; i < 256; i += 8)
    for (imm = 0; imm < 100; imm++)
      {
	/* Manually compute the result */
	compute_correct_result_64 (&vals[i + 0], &vals[i + 4], imm, ck);

	/* Run the 64-bit tests */
	ssse3_test_palignr (&vals[i + 0], &vals[i + 4], imm, &r[0]);
	ssse3_test_palignr (&vals[i + 2], &vals[i + 6], imm, &r[2]);
	fail += chk_128 (ck, r);

	/* Recompute the results for 128-bits */
	compute_correct_result_128 (&vals[i + 0], &vals[i + 4], imm, ck);

	/* Run the 128-bit tests */
	ssse3_test_palignr128 (&vals[i + 0], &vals[i + 4], imm, r);
	fail += chk_128 (ck, r);
      }

  if (fail != 0)
    abort ();
}
