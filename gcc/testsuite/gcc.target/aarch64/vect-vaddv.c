/* { dg-do run } */
/* { dg-options "-O3 --save-temps -ffast-math" } */

#pragma GCC target "+nosve"

#include <arm_neon.h>

extern void abort (void);
extern float fabsf (float);
extern double fabs (double);

#define NUM_TESTS 16
#define DELTA 0.000001

int8_t input_int8[] = {1, 56, 2, -9, -90, 23, 54, 76,
		       -4, 34, 110, -110, 6, 4, 75, -34};
int16_t input_int16[] = {1, 56, 2, -9, -90, 23, 54, 76,
			 -4, 34, 110, -110, 6, 4, 75, -34};
int32_t input_int32[] = {1, 56, 2, -9, -90, 23, 54, 76,
			 -4, 34, 110, -110, 6, 4, 75, -34};
int64_t input_int64[] = {1, 56, 2, -9, -90, 23, 54, 76,
			 -4, 34, 110, -110, 6, 4, 75, -34};

uint8_t input_uint8[] = {1, 56, 2, 9, 90, 23, 54, 76,
			 4, 34, 110, 110, 6, 4, 75, 34};
uint16_t input_uint16[] = {1, 56, 2, 9, 90, 23, 54, 76,
			   4, 34, 110, 110, 6, 4, 75, 34};
uint32_t input_uint32[] = {1, 56, 2, 9, 90, 23, 54, 76,
			   4, 34, 110, 110, 6, 4, 75, 34};

uint64_t input_uint64[] = {1, 56, 2, 9, 90, 23, 54, 76,
			   4, 34, 110, 110, 6, 4, 75, 34};

float input_float32[] = {0.1f, -0.1f, 0.4f, 10.3f,
			 200.0f, -800.0f, -13.0f, -0.5f,
			 7.9f, -870.0f, 10.4f, 310.11f,
			 0.0f, -865.0f, -2213.0f, -1.5f};

double input_float64[] = {0.1, -0.1, 0.4, 10.3,
			  200.0, -800.0, -13.0, -0.5,
			  7.9, -870.0, 10.4, 310.11,
			  0.0, -865.0, -2213.0, -1.5};

#define EQUALF(a, b) (fabsf (a - b) < DELTA)
#define EQUALD(a, b) (fabs (a - b) < DELTA)
#define EQUALL(a, b) (a == b)

#define TEST(SUFFIX, Q, TYPE, LANES, FLOAT)				\
int									\
test_vaddv##SUFFIX##_##TYPE##x##LANES##_t (void)			\
{									\
  int i, j;								\
  int moves = (NUM_TESTS - LANES) + 1;					\
  TYPE##_t out_l[NUM_TESTS];						\
  TYPE##_t out_v[NUM_TESTS];						\
									\
  /* Calculate linearly.  */						\
  for (i = 0; i < moves; i++)						\
    {									\
      asm ("" : "=r" (out_l[i]) : "0" (0));				\
      for (j = 0; j < LANES; j++)					\
	out_l[i] += input_##TYPE[i + j];				\
    }									\
									\
  /* Calculate using vector reduction intrinsics.  */			\
  for (i = 0; i < moves; i++)						\
    {									\
      TYPE##x##LANES##_t t1 = vld1##Q##_##SUFFIX (input_##TYPE + i);	\
      out_v[i] = vaddv##Q##_##SUFFIX (t1);				\
    }									\
									\
  /* Compare.  */							\
  for (i = 0; i < moves; i++)						\
    {									\
      if (!EQUAL##FLOAT (out_v[i], out_l[i]))				\
	return 0;							\
    }									\
  return 1;								\
}

#define BUILD_VARIANTS(TYPE, STYPE, W32, W64, F)	\
TEST (STYPE,  , TYPE, W32, F)				\
TEST (STYPE, q, TYPE, W64, F)				\

BUILD_VARIANTS (int8, s8, 8, 16, L)
BUILD_VARIANTS (uint8, u8, 8, 16, L)
/* { dg-final { scan-assembler "addv\\tb\[0-9\]+, v\[0-9\]+\.8b" } } */
/* { dg-final { scan-assembler "addv\\tb\[0-9\]+, v\[0-9\]+\.16b" } } */
BUILD_VARIANTS (int16, s16, 4, 8, L)
BUILD_VARIANTS (uint16, u16, 4, 8, L)
/* { dg-final { scan-assembler "addv\\th\[0-9\]+, v\[0-9\]+\.4h" } } */
/* { dg-final { scan-assembler "addv\\th\[0-9\]+, v\[0-9\]+\.8h" } } */
BUILD_VARIANTS (int32, s32, 2, 4, L)
BUILD_VARIANTS (uint32, u32, 2, 4, L)
/* { dg-final { scan-assembler "addp\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s, v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "addv\\ts\[0-9\]+, v\[0-9\]+\.4s" } } */
TEST (s64, q, int64, 2, D)
TEST (u64, q, uint64, 2, D)
/* { dg-final { scan-assembler "addp\\td\[0-9\]+\, v\[0-9\]+\.2d" } } */

BUILD_VARIANTS (float32, f32, 2, 4, F)
/* { dg-final { scan-assembler "faddp\\ts\[0-9\]+, v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "faddp\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.4s" } } */
TEST (f64, q, float64, 2, D)
/* { dg-final { scan-assembler "faddp\\td\[0-9\]+\, v\[0-9\]+\.2d" } } */

#undef TEST
#define TEST(SUFFIX, Q, TYPE, LANES, FLOAT)		\
{							\
  if (!test_vaddv##SUFFIX##_##TYPE##x##LANES##_t ())	\
    abort ();						\
}

int
main (int argc, char **argv)
{
BUILD_VARIANTS (int8, s8, 8, 16, L)
BUILD_VARIANTS (uint8, u8, 8, 16, L)
BUILD_VARIANTS (int16, s16, 4, 8, L)
BUILD_VARIANTS (uint16, u16, 4, 8, L)
BUILD_VARIANTS (int32, s32, 2, 4, L)
BUILD_VARIANTS (uint32, u32, 2, 4, L)

BUILD_VARIANTS (float32, f32, 2, 4, F)
TEST (f64, q, float64, 2, D)

  return 0;
}

