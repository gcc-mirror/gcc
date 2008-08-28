/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.2" } */

#ifndef CHECK_H
#define CHECK_H "sse4_2-check.h"
#endif

#ifndef TEST
#define TEST sse4_2_test
#endif

#include CHECK_H

#include "sse4_2-pcmpstr.h"

#define NUM 1024

#define IMM_VAL0 \
  (_SIDD_SBYTE_OPS | _SIDD_CMP_RANGES | _SIDD_MASKED_POSITIVE_POLARITY)
#define IMM_VAL1 \
 (_SIDD_UBYTE_OPS | _SIDD_CMP_EQUAL_EACH | _SIDD_NEGATIVE_POLARITY \
  | _SIDD_MOST_SIGNIFICANT)
#define IMM_VAL2 \
 (_SIDD_UWORD_OPS | _SIDD_CMP_EQUAL_ANY | _SIDD_MASKED_NEGATIVE_POLARITY)
#define IMM_VAL3 \
  (_SIDD_SWORD_OPS | _SIDD_CMP_EQUAL_ORDERED \
   | _SIDD_MASKED_NEGATIVE_POLARITY | _SIDD_LEAST_SIGNIFICANT)


static void
TEST (void)
{
  union
    {
      __m128i x[NUM];
      char c[NUM *16];
    } src1, src2;
  int res, correct, correct_flags, l1, l2;
  int flags, cf, zf, sf, of, af;
  int i;

  for (i = 0; i < NUM *16; i++)
    {
      src1.c[i] = rand ();
      src2.c[i] = rand ();
    }

  for (i = 0; i < NUM; i++)
    {
      l1 = rand () % 18;
      l2 = rand () % 18;

      switch ((rand () % 4))
	{
	case 0:
	  res = _mm_cmpestri (src1.x[i], l1, src2.x[i], l2, IMM_VAL0);
	  cf = _mm_cmpestrc (src1.x[i], l1, src2.x[i], l2, IMM_VAL0);
	  zf = _mm_cmpestrz (src1.x[i], l1, src2.x[i], l2, IMM_VAL0);
	  sf = _mm_cmpestrs (src1.x[i], l1, src2.x[i], l2, IMM_VAL0);
	  of = _mm_cmpestro (src1.x[i], l1, src2.x[i], l2, IMM_VAL0);
	  af = _mm_cmpestra (src1.x[i], l1, src2.x[i], l2, IMM_VAL0);
	  correct = cmp_ei (&src1.x[i], l1, &src2.x[i], l2, IMM_VAL0,
			    &correct_flags);
	  break;

	case 1:
	  res = _mm_cmpestri (src1.x[i], l1, src2.x[i], l2, IMM_VAL1);
	  cf = _mm_cmpestrc (src1.x[i], l1, src2.x[i], l2, IMM_VAL1);
	  zf = _mm_cmpestrz (src1.x[i], l1, src2.x[i], l2, IMM_VAL1);
	  sf = _mm_cmpestrs (src1.x[i], l1, src2.x[i], l2, IMM_VAL1);
	  of = _mm_cmpestro (src1.x[i], l1, src2.x[i], l2, IMM_VAL1);
	  af = _mm_cmpestra (src1.x[i], l1, src2.x[i], l2, IMM_VAL1);
	  correct = cmp_ei (&src1.x[i], l1, &src2.x[i], l2, IMM_VAL1,
			    &correct_flags);
	  break;

	case 2:
	  res = _mm_cmpestri (src1.x[i], l1, src2.x[i], l2, IMM_VAL2);
	  cf = _mm_cmpestrc (src1.x[i], l1, src2.x[i], l2, IMM_VAL2);
	  zf = _mm_cmpestrz (src1.x[i], l1, src2.x[i], l2, IMM_VAL2);
	  sf = _mm_cmpestrs (src1.x[i], l1, src2.x[i], l2, IMM_VAL2);
	  of = _mm_cmpestro (src1.x[i], l1, src2.x[i], l2, IMM_VAL2);
	  af = _mm_cmpestra (src1.x[i], l1, src2.x[i], l2, IMM_VAL2);
	  correct = cmp_ei (&src1.x[i], l1, &src2.x[i], l2, IMM_VAL2,
			    &correct_flags);
	  break;

	default:
	  res = _mm_cmpestri (src1.x[i], l1, src2.x[i], l2, IMM_VAL3);
	  cf = _mm_cmpestrc (src1.x[i], l1, src2.x[i], l2, IMM_VAL3);
	  zf = _mm_cmpestrz (src1.x[i], l1, src2.x[i], l2, IMM_VAL3);
	  sf = _mm_cmpestrs (src1.x[i], l1, src2.x[i], l2, IMM_VAL3);
	  of = _mm_cmpestro (src1.x[i], l1, src2.x[i], l2, IMM_VAL3);
	  af = _mm_cmpestra (src1.x[i], l1, src2.x[i], l2, IMM_VAL3);
	  correct = cmp_ei (&src1.x[i], l1, &src2.x[i], l2, IMM_VAL3,
			    &correct_flags);
	  break;
	}
      
      if (correct != res)
	abort ();

      flags = 0;
      if (cf)
	flags |= CFLAG;
      if (zf)
	flags |= ZFLAG;
      if (sf)
	flags |= SFLAG;
      if (of)
	flags |= OFLAG;
      
      if (flags != correct_flags
	  || (af && (cf || zf))
	  || (!af && !(cf || zf)))
	abort ();
    }
}
