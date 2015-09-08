/* { dg-do run } */
/* { dg-options "-O3 -std=c99" } */

#include <arm_neon.h>

extern void abort (void);

#define VARIANTS(VARIANT)				\
VARIANT (uint8_t, 8, uint8x8_t, uint8x16_t, u8)		\
VARIANT (uint16_t, 4, uint16x4_t, uint16x8_t, u16)	\
VARIANT (uint32_t, 2, uint32x2_t, uint32x4_t, u32)	\
VARIANT (uint64_t, 1, uint64x1_t, uint64x2_t, u64)	\
VARIANT (int8_t, 8, int8x8_t, int8x16_t, s8)		\
VARIANT (int16_t, 4, int16x4_t, int16x8_t, s16)		\
VARIANT (int32_t, 2, int32x2_t, int32x4_t, s32)		\
VARIANT (int64_t, 1, int64x1_t, int64x2_t, s64)		\
VARIANT (float16_t, 4, float16x4_t, float16x8_t, f16)	\
VARIANT (float32_t, 2, float32x2_t, float32x4_t, f32)	\
VARIANT (float64_t, 1, float64x1_t, float64x2_t, f64)


#define TESTMETH(BASETYPE, NUM64, TYPE64, TYPE128, SUFFIX)	\
int								\
test_vget_low_ ##SUFFIX (BASETYPE *data)			\
{								\
  BASETYPE temp [NUM64];					\
  TYPE128 vec = vld1q_##SUFFIX (data);				\
  TYPE64 high = vget_high_##SUFFIX (vec);			\
  vst1_##SUFFIX (temp, high);					\
  for (int i = 0; i < NUM64; i++)				\
    if (temp[i] != data[i + NUM64])				\
      return 1;							\
  return 0;							\
}

VARIANTS (TESTMETH)

#define CHECK(BASETYPE, NUM64, TYPE64, TYPE128, SUFFIX)		\
  if (test_vget_low_##SUFFIX (BASETYPE ## _ ## data) != 0)	\
    abort ();

int
main (int argc, char **argv)
{
  uint8_t uint8_t_data[16] =
      { 1, 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47 };
  uint16_t uint16_t_data[8] = { 1, 22, 333, 4444, 55555, 6666, 777, 88 };
  uint32_t uint32_t_data[4] = { 65537, 11, 70000, 23 };
  uint64_t uint64_t_data[2] = { 0xdeadbeefcafebabeULL, 0x0123456789abcdefULL };
  int8_t int8_t_data[16] =
      { -1, -3, -5, -7, 9, -11, -13, 15, -17, -19, 21, -23, 25, 27, -29, -31 };
  int16_t int16_t_data[8] = { -17, 19, 3, -999, 44048, 505, 9999, 1000};
  int32_t int32_t_data[4] = { 123456789, -987654321, -135792468, 975318642 };
  int64_t int64_t_data[2] = {0xfedcba9876543210LL, 0xdeadbabecafebeefLL };
  float16_t float16_t_data[8] = { 1.25, 4.5, 7.875, 2.3125, 5.675, 8.875,
      3.6875, 6.75};
  float32_t float32_t_data[4] = { 3.14159, 2.718, 1.414, 100.0 };
  float64_t float64_t_data[2] = { 1.01001000100001, 12345.6789 };

  VARIANTS (CHECK);

  return 0;
}
