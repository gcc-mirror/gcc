/* { dg-do run } */
/* { dg-options "-O3 -mvsx -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target p8vector_hw } */

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

#ifndef __AVX__
/* Test the 64-bit form */
static void
ssse3_test_palignr (__m64 *i1, __m64 *i2, unsigned int imm, __m64 *r)
{
  switch (imm)
    {
    case 0:
      *r = _mm_alignr_pi8 (*i1, *i2, 0);
      break;
    case 1:
      *r = _mm_alignr_pi8 (*i1, *i2, 1);
      break;
    case 2:
      *r = _mm_alignr_pi8 (*i1, *i2, 2);
      break;
    case 3:
      *r = _mm_alignr_pi8 (*i1, *i2, 3);
      break;
    case 4:
      *r = _mm_alignr_pi8 (*i1, *i2, 4);
      break;
    case 5:
      *r = _mm_alignr_pi8 (*i1, *i2, 5);
      break;
    case 6:
      *r = _mm_alignr_pi8 (*i1, *i2, 6);
      break;
    case 7:
      *r = _mm_alignr_pi8 (*i1, *i2, 7);
      break;
    case 8:
      *r = _mm_alignr_pi8 (*i1, *i2, 8);
      break;
    case 9:
      *r = _mm_alignr_pi8 (*i1, *i2, 9);
      break;
    case 10:
      *r = _mm_alignr_pi8 (*i1, *i2, 10);
      break;
    case 11:
      *r = _mm_alignr_pi8 (*i1, *i2, 11);
      break;
    case 12:
      *r = _mm_alignr_pi8 (*i1, *i2, 12);
      break;
    case 13:
      *r = _mm_alignr_pi8 (*i1, *i2, 13);
      break;
    case 14:
      *r = _mm_alignr_pi8 (*i1, *i2, 14);
      break;
    case 15:
      *r = _mm_alignr_pi8 (*i1, *i2, 15);
      break;
    default:
      *r = _mm_alignr_pi8 (*i1, *i2, 16);
      break;
    }

   _mm_empty();
}
#endif

/* Test the 128-bit form */
static void
ssse3_test_palignr128 (__m128i *i1, __m128i *i2, unsigned int imm, __m128i *r)
{
  /* Assumes incoming pointers are 16-byte aligned */

  switch (imm)
    {
    case 0:
      *r = _mm_alignr_epi8 (*i1, *i2, 0);
      break;
    case 1:
      *r = _mm_alignr_epi8 (*i1, *i2, 1);
      break;
    case 2:
      *r = _mm_alignr_epi8 (*i1, *i2, 2);
      break;
    case 3:
      *r = _mm_alignr_epi8 (*i1, *i2, 3);
      break;
    case 4:
      *r = _mm_alignr_epi8 (*i1, *i2, 4);
      break;
    case 5:
      *r = _mm_alignr_epi8 (*i1, *i2, 5);
      break;
    case 6:
      *r = _mm_alignr_epi8 (*i1, *i2, 6);
      break;
    case 7:
      *r = _mm_alignr_epi8 (*i1, *i2, 7);
      break;
    case 8:
      *r = _mm_alignr_epi8 (*i1, *i2, 8);
      break;
    case 9:
      *r = _mm_alignr_epi8 (*i1, *i2, 9);
      break;
    case 10:
      *r = _mm_alignr_epi8 (*i1, *i2, 10);
      break;
    case 11:
      *r = _mm_alignr_epi8 (*i1, *i2, 11);
      break;
    case 12:
      *r = _mm_alignr_epi8 (*i1, *i2, 12);
      break;
    case 13:
      *r = _mm_alignr_epi8 (*i1, *i2, 13);
      break;
    case 14:
      *r = _mm_alignr_epi8 (*i1, *i2, 14);
      break;
    case 15:
      *r = _mm_alignr_epi8 (*i1, *i2, 15);
      break;
    case 16:
      *r = _mm_alignr_epi8 (*i1, *i2, 16);
      break;
    case 17:
      *r = _mm_alignr_epi8 (*i1, *i2, 17);
      break;
    case 18:
      *r = _mm_alignr_epi8 (*i1, *i2, 18);
      break;
    case 19:
      *r = _mm_alignr_epi8 (*i1, *i2, 19);
      break;
    case 20:
      *r = _mm_alignr_epi8 (*i1, *i2, 20);
      break;
    case 21:
      *r = _mm_alignr_epi8 (*i1, *i2, 21);
      break;
    case 22:
      *r = _mm_alignr_epi8 (*i1, *i2, 22);
      break;
    case 23:
      *r = _mm_alignr_epi8 (*i1, *i2, 23);
      break;
    case 24:
      *r = _mm_alignr_epi8 (*i1, *i2, 24);
      break;
    case 25:
      *r = _mm_alignr_epi8 (*i1, *i2, 25);
      break;
    case 26:
      *r = _mm_alignr_epi8 (*i1, *i2, 26);
      break;
    case 27:
      *r = _mm_alignr_epi8 (*i1, *i2, 27);
      break;
    case 28:
      *r = _mm_alignr_epi8 (*i1, *i2, 28);
      break;
    case 29:
      *r = _mm_alignr_epi8 (*i1, *i2, 29);
      break;
    case 30:
      *r = _mm_alignr_epi8 (*i1, *i2, 30);
      break;
    case 31:
      *r = _mm_alignr_epi8 (*i1, *i2, 31);
      break;
    default:
      *r = _mm_alignr_epi8 (*i1, *i2, 32);
      break;
    }
}

/* Routine to manually compute the results */
static void
compute_correct_result_128 (signed char *i1, signed char *i2, unsigned int imm,
                            signed char *r)
{
  signed char buf [32];
  int i;

  memcpy (&buf[0], i2, 16);
  memcpy (&buf[16], i1, 16);

  for (i = 0; i < 16; i++)
    if (imm >= 32 || imm + i >= 32)
      r[i] = 0;
    else
      r[i] = buf[imm + i];
}

#ifndef __AVX__
static void
compute_correct_result_64 (signed char *i1, signed char *i2, unsigned int imm,
                           signed char *r)
{
  signed char buf [16];
  int i;

  /* Handle the first half */
  memcpy (&buf[0], &i2[0], 8);
  memcpy (&buf[8], &i1[0], 8);

  for (i = 0; i < 8; i++)
    if (imm >= 16 || imm + i >= 16)
      r[i] = 0;
    else
      r[i] = buf[imm + i];

  /* Handle the second half */
  memcpy (&buf[0], &i2[8], 8);
  memcpy (&buf[8], &i1[8], 8);

  for (i = 0; i < 8; i++)
    if (imm >= 16 || imm + i >= 16)
      r[i + 8] = 0;
    else
      r[i + 8] = buf[imm + i];
}
#endif

static void
TEST (void)
{
  int i;
  union data r __attribute__ ((aligned(16)));
  union data ck;
  unsigned int imm;
  int fail = 0;

  for (i = 0; i < ARRAY_SIZE (vals) - 1; i++)
    for (imm = 0; imm < 100; imm++)
      {
#ifndef __AVX__
	/* Manually compute the result */
	compute_correct_result_64 (&vals[i + 0].b[0],
                                   &vals[i + 1].b[0], imm, &ck.b[0]);

	/* Run the 64-bit tests */
	ssse3_test_palignr (&vals[i + 0].ll[0],
                            &vals[i + 1].ll[0], imm, &r.ll[0]);
	ssse3_test_palignr (&vals[i + 0].ll[1],
                            &vals[i + 1].ll[1], imm, &r.ll[1]);
	fail += chk_128 (ck.m[0], r.m[0]);
#endif

	/* Recompute the results for 128-bits */
	compute_correct_result_128 (&vals[i + 0].b[0],
                                    &vals[i + 1].b[0], imm, &ck.b[0]);

	/* Run the 128-bit tests */
	ssse3_test_palignr128 (&vals[i + 0].m[0],
                               &vals[i + 1].m[0], imm, &r.m[0]);
	fail += chk_128 (ck.m[0], r.m[0]);
      }

  if (fail != 0)
    abort ();
}
