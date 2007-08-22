/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.2" } */

#include "sse4_2-check.h"
#include "sse4_2-pcmpstr.h"

#define NUM 1024

#define IMM_VAL0 \
  (SIDD_SBYTE_OPS | SIDD_CMP_RANGES | SIDD_MASKED_POSITIVE_POLARITY)
#define IMM_VAL1 \
 (SIDD_UBYTE_OPS | SIDD_CMP_EQUAL_EACH | SIDD_NEGATIVE_POLARITY \
  | SIDD_MOST_SIGNIFICANT)
#define IMM_VAL2 \
 (SIDD_UWORD_OPS | SIDD_CMP_EQUAL_ANY | SIDD_MASKED_NEGATIVE_POLARITY)
#define IMM_VAL3 \
  (SIDD_SWORD_OPS | SIDD_CMP_EQUAL_ORDERED \
   | SIDD_MASKED_NEGATIVE_POLARITY | SIDD_MOST_SIGNIFICANT)


static void
sse4_2_test (void)
{
  union
    {
      __m128i x[NUM];
      char c[NUM *16];
    } src1, src2;
  int res, correct;
  int i;

  for (i = 0; i < NUM *16; i++)
    {
      src1.c[i] = rand ();
      src2.c[i] = rand ();
    }

  for (i = 0; i < NUM; i++)
    {
      switch ((rand () % 4))
	{
	case 0:
	  res = _mm_cmpistri (src1.x[i], src2.x[i], IMM_VAL0);
	  correct = cmp_ii (&src1.x[i], &src2.x[i], IMM_VAL0, NULL);
	  break;

	case 1:
	  res = _mm_cmpistri (src1.x[i], src2.x[i], IMM_VAL1);
	  correct = cmp_ii (&src1.x[i], &src2.x[i], IMM_VAL1, NULL);
	  break;

	case 2:
	  res = _mm_cmpistri (src1.x[i], src2.x[i], IMM_VAL2);
	  correct = cmp_ii (&src1.x[i], &src2.x[i], IMM_VAL2, NULL);
	  break;

	default:
	  res = _mm_cmpistri (src1.x[i], src2.x[i], IMM_VAL3);
	  correct = cmp_ii (&src1.x[i], &src2.x[i], IMM_VAL3, NULL);
	  break;
        }

      if (correct != res)
	abort ();
    }
}
