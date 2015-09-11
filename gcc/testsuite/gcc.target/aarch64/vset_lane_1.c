/* { dg-do run } */
/* { dg-options "-O3 -fno-inline" } */

#include <arm_neon.h>

extern void abort (void);

#define VARIANTS(VARIANT)			\
VARIANT (uint8_t, , 8, uint8x8_t, _u8, 5)	\
VARIANT (uint16_t, , 4, uint16x4_t, _u16, 3)	\
VARIANT (uint32_t, , 2, uint32x2_t, _u32, 1)	\
VARIANT (uint64_t, , 1, uint64x1_t, _u64, 0)	\
VARIANT (int8_t, , 8, int8x8_t, _s8, 6)		\
VARIANT (int16_t, , 4, int16x4_t, _s16, 2)	\
VARIANT (int32_t, , 2, int32x2_t, _s32, 0)	\
VARIANT (int64_t, , 1, int64x1_t, _s64, 0)	\
VARIANT (poly8_t, , 8, poly8x8_t, _p8, 6)	\
VARIANT (poly16_t, , 4, poly16x4_t, _p16, 2)	\
VARIANT (float16_t, , 4, float16x4_t, _f16, 3)	\
VARIANT (float32_t, , 2, float32x2_t, _f32, 1)	\
VARIANT (float64_t, , 1, float64x1_t, _f64, 0)	\
VARIANT (uint8_t, q, 16, uint8x16_t, _u8, 11)	\
VARIANT (uint16_t, q, 8, uint16x8_t, _u16, 7)	\
VARIANT (uint32_t, q, 4, uint32x4_t, _u32, 2)	\
VARIANT (uint64_t, q, 2, uint64x2_t, _u64, 1)	\
VARIANT (int8_t, q, 16, int8x16_t, _s8, 13)	\
VARIANT (int16_t, q, 8, int16x8_t, _s16, 5)	\
VARIANT (int32_t, q, 4, int32x4_t, _s32, 3)	\
VARIANT (int64_t, q, 2, int64x2_t, _s64, 0)	\
VARIANT (poly8_t, q, 16, poly8x16_t, _p8, 14)	\
VARIANT (poly16_t, q, 8, poly16x8_t, _p16, 6)	\
VARIANT (float16_t, q, 8, float16x8_t, _f16, 6)	\
VARIANT (float32_t, q, 4, float32x4_t, _f32, 2) \
VARIANT (float64_t, q, 2, float64x2_t, _f64, 1)

#define TESTMETH(BASETYPE, Q, NUM, TYPE, SUFFIX, INDEX)	\
int							\
test_vset_lane ##Q##SUFFIX (BASETYPE *data)		\
{							\
  BASETYPE temp [NUM];					\
  TYPE vec = vld1##Q##SUFFIX (data);			\
  TYPE vec2;						\
  BASETYPE changed = data[INDEX] - INDEX;		\
  int check;						\
  vec = vset##Q##_lane##SUFFIX (changed, vec, INDEX);	\
  asm volatile ("orr %0.16b, %1.16b, %1.16b"		\
		: "=w"(vec2) : "w" (vec) : );		\
  vst1##Q##SUFFIX (temp, vec2);				\
  for (check = 0; check < NUM; check++)			\
    {							\
      BASETYPE desired = data[check];			\
      if (check==INDEX) desired = changed;		\
      if (temp[check] != desired)			\
        return 1;					\
    }							\
  return 0;						\
}

VARIANTS (TESTMETH)

#define CHECK(BASETYPE, Q, NUM, TYPE, SUFFIX, INDEX)		\
  if (test_vset_lane##Q##SUFFIX (BASETYPE ## _ ## data) != 0)	\
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
  poly8_t poly8_t_data[16] =
      { 0, 7, 13, 18, 22, 25, 27, 28, 29, 31, 34, 38, 43, 49, 56, 64 };
  poly16_t poly16_t_data[8] = { 11111, 2222, 333, 44, 5, 65432, 54321, 43210 };
  float16_t float16_t_data[8] = { 1.25, 4.5, 7.875, 2.3125, 5.675, 8.875,
      3.6875, 6.75};

  float32_t float32_t_data[4] = { 3.14159, 2.718, 1.414, 100.0 };
  float64_t float64_t_data[2] = { 1.01001000100001, 12345.6789 };

  VARIANTS (CHECK);

  return 0;
}
