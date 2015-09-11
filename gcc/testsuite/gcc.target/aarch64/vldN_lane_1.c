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
VARIANT (float16, , 4, _f16, 3, STRUCT)	\
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
VARIANT (float16, q, 8, _f16, 7, STRUCT)\
VARIANT (float32, q, 4, _f32, 1, STRUCT)\
VARIANT (float64, q, 2, _f64, 0, STRUCT)

#define TESTMETH(BASE, Q, ELTS, SUFFIX, LANE, STRUCT)			\
int									\
test_vld##STRUCT##Q##_lane##SUFFIX (const BASE##_t *data,		\
				     const BASE##_t *overwrite)		\
{									\
  BASE##x##ELTS##x##STRUCT##_t vectors;					\
  BASE##_t temp[ELTS];							\
  int i,j;								\
  for (i = 0; i < STRUCT; i++, data += ELTS)				\
    vectors.val[i] = vld1##Q##SUFFIX (data);				\
  vectors = vld##STRUCT##Q##_lane##SUFFIX (overwrite, vectors, LANE);	\
  while (--i >= 0)							\
    {									\
      vst1##Q##SUFFIX (temp, vectors.val[i]);				\
      data -= ELTS; /* Point at value loaded before vldN_lane.  */	\
      for (j = 0; j < ELTS; j++)					\
        if (temp[j] != (j == LANE ? overwrite[i] : data[j]))		\
          return 1;							\
    }									\
  return 0;								\
}


/* Tests of vld2_lane and vld2q_lane.  */
VARIANTS (TESTMETH, 2)
/* Tests of vld3_lane and vld3q_lane.  */
VARIANTS (TESTMETH, 3)
/* Tests of vld4_lane and vld4q_lane.  */
VARIANTS (TESTMETH, 4)

#define CHECK(BASE, Q, ELTS, SUFFIX, LANE, STRUCT)			\
  if (test_vld##STRUCT##Q##_lane##SUFFIX ((const BASE##_t *)orig_data,	\
						BASE##_data) != 0)	\
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
  uint8_t uint8_data[4] = { 7, 11, 13, 17 };
  uint16_t uint16_data[4] = { 257, 263, 269, 271 };
  uint32_t uint32_data[4] = { 65537, 65539, 65543, 65551 };
  uint64_t uint64_data[4] = { 0xdeadbeefcafebabeULL, 0x0123456789abcdefULL,
			      0xfedcba9876543210LL, 0xdeadbabecafebeefLL };
  int8_t int8_data[4] = { -1, 3, -5, 7 };
  int16_t int16_data[4] = { 257, -259, 261, -263 };
  int32_t int32_data[4] = { 123456789, -987654321, -135792468, 975318642 };
  int64_t *int64_data = (int64_t *)uint64_data;
  poly8_t poly8_data[4] = { 0, 7, 13, 18, };
  poly16_t poly16_data[4] = { 11111, 2222, 333, 44 };
  float16_t float16_data[4] = { 0.8125, 7.5, 19, 0.046875 };
  float32_t float32_data[4] = { 3.14159, 2.718, 1.414, 100.0 };
  float64_t float64_data[4] = { 1.010010001, 12345.6789, -9876.54321, 1.618 };

  VARIANTS (CHECK, 2);
  VARIANTS (CHECK, 3);
  VARIANTS (CHECK, 4);
  return 0;
}
