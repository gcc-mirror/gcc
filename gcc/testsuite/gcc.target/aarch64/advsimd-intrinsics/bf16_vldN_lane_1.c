/* { dg-do run { target { aarch64*-*-* } } } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon }  */

#include <arm_neon.h>

extern void abort (void);

typedef union
{
  bfloat16_t bf16;
  uint16_t u16;
} bfloat16_u_t;

#define VARIANTS(VARIANT, STRUCT)		\
VARIANT (bfloat16, , 4, _bf16, 3, STRUCT)	\
VARIANT (bfloat16, q, 8, _bf16, 7, STRUCT)

#define TESTMETH(BASE, Q, ELTS, SUFFIX, LANE, STRUCT)			       \
  int									       \
  test_vld##STRUCT##Q##_lane##SUFFIX (const bfloat16_u_t *data,		       \
				      const bfloat16_u_t *overwrite)	       \
  {									       \
    BASE##x##ELTS##x##STRUCT##_t vectors;				       \
    bfloat16_u_t temp[ELTS];						       \
    int i,j;								       \
    for (i = 0; i < STRUCT; i++, data += ELTS)				       \
      vectors.val[i] = vld1##Q##SUFFIX ((bfloat16_t *)data);		       \
    vectors = vld##STRUCT##Q##_lane##SUFFIX ((bfloat16_t *) overwrite,	       \
					     vectors, LANE);		       \
    while (--i >= 0)							       \
      {									       \
	vst1##Q##SUFFIX ((bfloat16_t *)temp, vectors.val[i]);		       \
	data -= ELTS; /* Point at value loaded before vldN_lane.  */	       \
	for (j = 0; j < ELTS; j++)					       \
	  if (temp[j].u16 != (j == LANE ? overwrite[i].u16 : data[j].u16))     \
	    return 1;							       \
      }									       \
    return 0;								       \
  }

/* Tests of vld2_lane and vld2q_lane.  */
VARIANTS (TESTMETH, 2)
/* Tests of vld3_lane and vld3q_lane.  */
VARIANTS (TESTMETH, 3)
/* Tests of vld4_lane and vld4q_lane.  */
VARIANTS (TESTMETH, 4)

#define CHECK(BASE, Q, ELTS, SUFFIX, LANE, STRUCT)			       \
  if (test_vld##STRUCT##Q##_lane##SUFFIX ((const bfloat16_u_t *)orig_data,     \
					  BASE##_data) != 0)		       \
    abort ();

int
main (int argc, char **argv)
{
  /* Original data for all vector formats.  */
  uint64_t orig_data[8] = {0x1234567890abcdefULL, 0x13579bdf02468aceULL,
			   0x012389ab4567cdefULL, 0xdeeddadacafe0431ULL,
			   0x1032547698badcfeULL, 0xbadbadbadbad0badULL,
			   0x0102030405060708ULL, 0x0f0e0d0c0b0a0908ULL};

  /* Data with which vldN_lane will overwrite some of previous.  */
  bfloat16_u_t bfloat16_data[4];
  bfloat16_data[0].u16 = 0xABAB;
  bfloat16_data[1].u16 = 0x0;
  bfloat16_data[2].u16 = 0xCAFE;
  bfloat16_data[3].u16 = 0x1234;

  VARIANTS (CHECK, 2);
  VARIANTS (CHECK, 3);
  VARIANTS (CHECK, 4);
  return 0;
}
