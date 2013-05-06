/* { dg-do run } */
/* { dg-options "-O3 --save-temps -ffast-math" } */

#include <arm_neon.h>

extern void abort (void);

#define NUM_TESTS 16
#define DELTA 0.000001

int8_t input_int8[] = {1, 56, 2, -9, -90, 23, 54, 76,
		       -4, 34, 110, -110, 6, 4, 75, -34};
int16_t input_int16[] = {1, 56, 2, -9, -90, 23, 54, 76,
			 -4, 34, 110, -110, 6, 4, 75, -34};
int32_t input_int32[] = {1, 56, 2, -9, -90, 23, 54, 76,
			 -4, 34, 110, -110, 6, 4, 75, -34};

uint8_t input_uint8[] = {1, 56, 2, 9, 90, 23, 54, 76,
			 4, 34, 110, 110, 6, 4, 75, 34};
uint16_t input_uint16[] = {1, 56, 2, 9, 90, 23, 54, 76,
			   4, 34, 110, 110, 6, 4, 75, 34};
uint32_t input_uint32[] = {1, 56, 2, 9, 90, 23, 54, 76,
			   4, 34, 110, 110, 6, 4, 75, 34};

#define EQUAL(a, b) (a == b)

#define TEST(MAXMIN, CMP_OP, SUFFIX, Q, TYPE, LANES)			\
int									\
test_v##MAXMIN##v##SUFFIX##_##TYPE##x##LANES##_t (void)			\
{									\
  int i, j;								\
  int moves = (NUM_TESTS - LANES) + 1;					\
  TYPE##_t out_l[NUM_TESTS];						\
  TYPE##_t out_v[NUM_TESTS];						\
									\
  /* Calculate linearly.  */						\
  for (i = 0; i < moves; i++)						\
    {									\
      out_l[i] = input_##TYPE[i];					\
      for (j = 0; j < LANES; j++)					\
	out_l[i] = input_##TYPE[i + j] CMP_OP out_l[i]  ?		\
	  input_##TYPE[i + j] : out_l[i];				\
    }									\
									\
  /* Calculate using vector reduction intrinsics.  */			\
  for (i = 0; i < moves; i++)						\
    {									\
      TYPE##x##LANES##_t t1 = vld1##Q##_##SUFFIX (input_##TYPE + i);	\
      out_v[i] = v##MAXMIN##v##Q##_##SUFFIX (t1);			\
    }									\
									\
  /* Compare.  */							\
  for (i = 0; i < moves; i++)						\
    {									\
      if (!EQUAL (out_v[i], out_l[i]))					\
	return 0;							\
    }									\
  return 1;								\
}

#define BUILD_VARIANTS(TYPE, STYPE, W32, W64)		\
TEST (max, >, STYPE,  , TYPE, W32)			\
TEST (max, >, STYPE, q, TYPE, W64)			\
TEST (min, <, STYPE,  , TYPE, W32)			\
TEST (min, <, STYPE, q, TYPE, W64)

BUILD_VARIANTS (int8, s8, 8, 16)
/* { dg-final { scan-assembler "smaxv\\tb\[0-9\]+, v\[0-9\]+\.8b" } } */
/* { dg-final { scan-assembler "sminv\\tb\[0-9\]+, v\[0-9\]+\.8b" } } */
/* { dg-final { scan-assembler "smaxv\\tb\[0-9\]+, v\[0-9\]+\.16b" } } */
/* { dg-final { scan-assembler "sminv\\tb\[0-9\]+, v\[0-9\]+\.16b" } } */
BUILD_VARIANTS (uint8, u8, 8, 16)
/* { dg-final { scan-assembler "umaxv\\tb\[0-9\]+, v\[0-9\]+\.8b" } } */
/* { dg-final { scan-assembler "uminv\\tb\[0-9\]+, v\[0-9\]+\.8b" } } */
/* { dg-final { scan-assembler "umaxv\\tb\[0-9\]+, v\[0-9\]+\.16b" } } */
/* { dg-final { scan-assembler "uminv\\tb\[0-9\]+, v\[0-9\]+\.16b" } } */
BUILD_VARIANTS (int16, s16, 4, 8)
/* { dg-final { scan-assembler "smaxv\\th\[0-9\]+, v\[0-9\]+\.4h" } } */
/* { dg-final { scan-assembler "sminv\\th\[0-9\]+, v\[0-9\]+\.4h" } } */
/* { dg-final { scan-assembler "smaxv\\th\[0-9\]+, v\[0-9\]+\.8h" } } */
/* { dg-final { scan-assembler "sminv\\th\[0-9\]+, v\[0-9\]+\.8h" } } */
BUILD_VARIANTS (uint16, u16, 4, 8)
/* { dg-final { scan-assembler "umaxv\\th\[0-9\]+, v\[0-9\]+\.4h" } } */
/* { dg-final { scan-assembler "uminv\\th\[0-9\]+, v\[0-9\]+\.4h" } } */
/* { dg-final { scan-assembler "umaxv\\th\[0-9\]+, v\[0-9\]+\.8h" } } */
/* { dg-final { scan-assembler "uminv\\th\[0-9\]+, v\[0-9\]+\.8h" } } */
BUILD_VARIANTS (int32, s32, 2, 4)
/* { dg-final { scan-assembler "smaxp\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s, v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "sminp\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s, v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "smaxv\\ts\[0-9\]+, v\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "sminv\\ts\[0-9\]+, v\[0-9\]+\.4s" } } */
BUILD_VARIANTS (uint32, u32, 2, 4)
/* { dg-final { scan-assembler "umaxp\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s, v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "uminp\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s, v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "umaxv\\ts\[0-9\]+, v\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "uminv\\ts\[0-9\]+, v\[0-9\]+\.4s" } } */

#undef TEST
#define TEST(MAXMIN, CMP_OP, SUFFIX, Q, TYPE, LANES)		\
{								\
  if (!test_v##MAXMIN##v##SUFFIX##_##TYPE##x##LANES##_t ())	\
    abort ();							\
}

int
main (int argc, char **argv)
{
  BUILD_VARIANTS (int8, s8, 8, 16)
  BUILD_VARIANTS (uint8, u8, 8, 16)
  BUILD_VARIANTS (int16, s16, 4, 8)
  BUILD_VARIANTS (uint16, u16, 4, 8)
  BUILD_VARIANTS (int32, s32, 2, 4)
  BUILD_VARIANTS (uint32, u32, 2, 4)
  return 0;
}

/* { dg-final { cleanup-saved-temps } } */
