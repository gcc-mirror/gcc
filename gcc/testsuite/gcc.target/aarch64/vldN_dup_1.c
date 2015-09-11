/* { dg-do run } */
/* { dg-options "-O3 -fno-inline" } */

#include <arm_neon.h>

extern void abort (void);

#define VARIANTS(VARIANT, STRUCT)	\
VARIANT (uint8, , 8, _u8, STRUCT)	\
VARIANT (uint16, , 4, _u16, STRUCT)	\
VARIANT (uint32, , 2, _u32, STRUCT)	\
VARIANT (uint64, , 1, _u64, STRUCT)	\
VARIANT (int8, , 8, _s8, STRUCT)	\
VARIANT (int16, , 4, _s16, STRUCT)	\
VARIANT (int32, , 2, _s32, STRUCT)	\
VARIANT (int64, , 1, _s64, STRUCT)	\
VARIANT (poly8, , 8, _p8, STRUCT)	\
VARIANT (poly16, , 4, _p16, STRUCT)	\
VARIANT (float16, , 4, _f16, STRUCT)	\
VARIANT (float32, , 2, _f32, STRUCT)	\
VARIANT (float64, , 1, _f64, STRUCT)	\
VARIANT (uint8, q, 16, _u8, STRUCT)	\
VARIANT (uint16, q, 8, _u16, STRUCT)	\
VARIANT (uint32, q, 4, _u32, STRUCT)	\
VARIANT (uint64, q, 2, _u64, STRUCT)	\
VARIANT (int8, q, 16, _s8, STRUCT)	\
VARIANT (int16, q, 8, _s16, STRUCT)	\
VARIANT (int32, q, 4, _s32, STRUCT)	\
VARIANT (int64, q, 2, _s64, STRUCT)	\
VARIANT (poly8, q, 16, _p8, STRUCT)	\
VARIANT (poly16, q, 8, _p16, STRUCT)	\
VARIANT (float16, q, 8, _f16, STRUCT)	\
VARIANT (float32, q, 4, _f32, STRUCT)	\
VARIANT (float64, q, 2, _f64, STRUCT)

#define TESTMETH(BASE, Q, ELTS, SUFFIX, STRUCT)	\
int								\
test_vld##STRUCT##Q##_dup##SUFFIX (const BASE##_t *data)	\
{								\
  BASE##_t temp[ELTS];						\
  BASE##x##ELTS##x##STRUCT##_t vectors =			\
			   vld##STRUCT##Q##_dup##SUFFIX (data); \
  int i,j;							\
  for (i = 0; i < STRUCT; i++)					\
    {								\
      vst1##Q##SUFFIX (temp, vectors.val[i]);			\
      for (j = 0; j < ELTS; j++)				\
        if (temp[j] != data[i])					\
          return 1;						\
    }								\
  return 0;							\
}

/* Tests of vld2_dup and vld2q_dup.  */
VARIANTS (TESTMETH, 2)
/* Tests of vld3_dup and vld3q_dup.  */
VARIANTS (TESTMETH, 3)
/* Tests of vld4_dup and vld4q_dup.  */
VARIANTS (TESTMETH, 4)

#define CHECK(BASE, Q, ELTS, SUFFIX, STRUCT)			\
  if (test_vld##STRUCT##Q##_dup##SUFFIX (BASE ##_data) != 0)	\
    abort ();

int
main (int argc, char **argv)
{
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
  float16_t float16_data[4] = { 1.0625, 3.125, 0.03125, 7.75 };
  float32_t float32_data[4] = { 3.14159, 2.718, 1.414, 100.0 };
  float64_t float64_data[4] = { 1.010010001, 12345.6789, -9876.54321, 1.618 };

  VARIANTS (CHECK, 2);
  VARIANTS (CHECK, 3);
  VARIANTS (CHECK, 4);
  return 0;
}
