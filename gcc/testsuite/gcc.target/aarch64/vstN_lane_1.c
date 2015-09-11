/* { dg-do run } */
/* { dg-options "-O3 -fno-inline" } */

#include <arm_neon.h>

extern void abort (void);

#define VARIANTS(VARIANT, STRUCT)	\
VARIANT (uint8, , 8, _u8, 6, STRUCT)	\
VARIANT (uint16, , 4, _u16, 3, STRUCT)	\
VARIANT (uint32, , 2, _u32, 1, STRUCT)	\
VARIANT (uint64, , 1, _u64, 0, STRUCT)	\
VARIANT (int8, , 8, _s8, 5, STRUCT)	\
VARIANT (int16, , 4, _s16, 2, STRUCT)	\
VARIANT (int32, , 2, _s32, 0, STRUCT)	\
VARIANT (int64, , 1, _s64, 0, STRUCT)	\
VARIANT (poly8, , 8, _p8, 7, STRUCT)	\
VARIANT (poly16, , 4, _p16, 1, STRUCT)	\
VARIANT (float32, , 2, _f32, 1, STRUCT)	\
VARIANT (float64, , 1, _f64, 0, STRUCT)	\
VARIANT (uint8, q, 16, _u8, 14, STRUCT)	\
VARIANT (uint16, q, 8, _u16, 4, STRUCT)	\
VARIANT (uint32, q, 4, _u32, 3, STRUCT)	\
VARIANT (uint64, q, 2, _u64, 0, STRUCT)	\
VARIANT (int8, q, 16, _s8, 13, STRUCT)	\
VARIANT (int16, q, 8, _s16, 6, STRUCT)	\
VARIANT (int32, q, 4, _s32, 2, STRUCT)	\
VARIANT (int64, q, 2, _s64, 1, STRUCT)	\
VARIANT (poly8, q, 16, _p8, 12, STRUCT)	\
VARIANT (poly16, q, 8, _p16, 5, STRUCT)	\
VARIANT (float32, q, 4, _f32, 1, STRUCT)\
VARIANT (float64, q, 2, _f64, 0, STRUCT)

#define TESTMETH(BASE, Q, ELTS, SUFFIX, LANE, STRUCT)			\
int									\
test_vst##STRUCT##Q##_lane##SUFFIX (const BASE##_t *data)		\
{									\
  BASE##x##ELTS##x##STRUCT##_t vectors;					\
  for (int i = 0; i < STRUCT; i++, data += ELTS)			\
    vectors.val[i] = vld1##Q##SUFFIX (data);				\
  BASE##_t temp[STRUCT];						\
  vst##STRUCT##Q##_lane##SUFFIX (temp, vectors, LANE);			\
  for (int i = 0; i < STRUCT; i++)					\
    {									\
      if (temp[i] != vget##Q##_lane##SUFFIX (vectors.val[i], LANE))	\
	return 1;							\
    }									\
  return 0;								\
}

/* Tests of vst2_lane and vst2q_lane.  */
VARIANTS (TESTMETH, 2)
/* Tests of vst3_lane and vst3q_lane.  */
VARIANTS (TESTMETH, 3)
/* Tests of vst4_lane and vst4q_lane.  */
VARIANTS (TESTMETH, 4)

#define CHECK(BASE, Q, ELTS, SUFFIX, LANE, STRUCT)			\
  if (test_vst##STRUCT##Q##_lane##SUFFIX ((const BASE##_t *)orig_data))	\
    abort ();

int
main (int argc, char **argv)
{
  /* Original data for all vector formats.  */
  uint64_t orig_data[8] = {0x1234567890abcdefULL, 0x13579bdf02468aceULL,
			   0x012389ab4567cdefULL, 0xfeeddadacafe0431ULL,
			   0x1032547698badcfeULL, 0xbadbadbadbad0badULL,
			   0x0102030405060708ULL, 0x0f0e0d0c0b0a0908ULL};

  VARIANTS (CHECK, 2);
  VARIANTS (CHECK, 3);
  VARIANTS (CHECK, 4);
  return 0;
}
