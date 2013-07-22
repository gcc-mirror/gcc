/* { dg-do run } */
/* { dg-options "-O3 --save-temps" } */

#include <arm_neon.h>

extern void abort (void);

#define ETYPE(size) int##size##_t
#define VTYPE(size, lanes) int##size##x##lanes##_t

#define TEST_VABS(q, size, lanes)				\
static void							\
test_vabs##q##_##size (ETYPE (size) * res,			\
			const ETYPE (size) *in1)		\
{								\
  VTYPE (size, lanes) a = vld1##q##_s##size (res);		\
  VTYPE (size, lanes) b = vld1##q##_s##size (in1);		\
  a = vabs##q##_s##size (b);					\
  vst1##q##_s##size (res, a);					\
}

#define BUILD_VARS(width, n_lanes, n_half_lanes)		\
TEST_VABS (, width, n_half_lanes)				\
TEST_VABS (q, width, n_lanes)					\

BUILD_VARS (64, 2, 1)
BUILD_VARS (32, 4, 2)
BUILD_VARS (16, 8, 4)
BUILD_VARS (8, 16, 8)

#define POOL1  {-10}
#define POOL2  {2, -10}
#define POOL4  {0, -10, 2, -3}
#define POOL8  {0, -10, 2, -3, 4, -50, 6, -70}
#define POOL16 {0, -10, 2, -3, 4, -50, 6, -70,			\
		-5, 10, -2, 3, -4, 50, -6, 70}

#define EXPECTED1  {10}
#define EXPECTED2  {2, 10}
#define EXPECTED4  {0, 10, 2, 3}
#define EXPECTED8  {0, 10, 2, 3, 4, 50, 6, 70}
#define EXPECTED16 {0, 10, 2, 3, 4, 50, 6, 70,			\
		    5, 10, 2, 3, 4, 50, 6, 70}

#define BUILD_TEST(size, lanes_64, lanes_128)			\
static void							\
test_##size (void)						\
{								\
  int i;							\
  ETYPE (size) pool1[lanes_64] = POOL##lanes_64;		\
  ETYPE (size) res1[lanes_64] = {0};				\
  ETYPE (size) expected1[lanes_64] = EXPECTED##lanes_64;	\
  ETYPE (size) pool2[lanes_128] = POOL##lanes_128;		\
  ETYPE (size) res2[lanes_128] = {0};				\
  ETYPE (size) expected2[lanes_128] = EXPECTED##lanes_128;	\
								\
  /* Forcefully avoid optimization.  */				\
  asm volatile ("" : : : "memory");				\
  test_vabs_##size (res1, pool1);				\
  for (i = 0; i < lanes_64; i++)				\
    if (res1[i] != expected1[i])				\
      abort ();							\
								\
  /* Forcefully avoid optimization.  */				\
  asm volatile ("" : : : "memory");				\
  test_vabsq_##size (res2, pool2);				\
  for (i = 0; i < lanes_128; i++)				\
    if (res2[i] != expected2[i])				\
      abort ();							\
}

/* { dg-final { scan-assembler-times "abs\\tv\[0-9\]+\.8b, v\[0-9\]+\.8b" 1 } } */
/* { dg-final { scan-assembler-times "abs\\tv\[0-9\]+\.16b, v\[0-9\]+\.16b" 1 } } */
BUILD_TEST (8 , 8, 16)

/* { dg-final { scan-assembler-times "abs\\tv\[0-9\]+\.4h, v\[0-9\]+\.4h" 1 } } */
/* { dg-final { scan-assembler-times "abs\\tv\[0-9\]+\.8h, v\[0-9\]+\.8h" 1 } } */
BUILD_TEST (16, 4, 8)

/* { dg-final { scan-assembler-times "abs\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s" 1 } } */
/* { dg-final { scan-assembler-times "abs\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s" 1 } } */
BUILD_TEST (32, 2, 4)

/* { dg-final { scan-assembler-times "abs\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d" 1 } } */
BUILD_TEST (64, 1, 2)

#undef BUILD_TEST

#define BUILD_TEST(size) test_##size ()

int
main (int argc, char **argv)
{
  BUILD_TEST (8);
  BUILD_TEST (16);
  BUILD_TEST (32);
  BUILD_TEST (64);
  return 0;
}

/* { dg-final { cleanup-saved-temps } } */
