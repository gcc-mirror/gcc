/* { dg-do run } */
/* { dg-options "-O3 --save-temps" } */

#include <arm_neon.h>

#define DELTA 0.0001
extern void abort (void);
extern double fabs (double);

#define TEST_VMUL(q1, q2, size, in1_lanes, in2_lanes)			\
static void								\
__attribute__((noipa,noinline))						\
test_vmul##q1##_lane##q2##_f##size (float##size##_t * res,		\
				   const float##size##_t *in1,		\
				   const float##size##_t *in2)		\
{									\
  float##size##x##in1_lanes##_t a = vld1##q1##_f##size (res);		\
  float##size##x##in1_lanes##_t b = vld1##q1##_f##size (in1);		\
  float##size##x##in2_lanes##_t c;					\
  if (in2_lanes > 1)							\
    {									\
      c = vld1##q2##_f##size (in2);					\
      a = vmul##q1##_lane##q2##_f##size (b, c, 1);			\
    }									\
  else									\
    {									\
      c = vld1##q2##_f##size (in2 + 1);					\
      a = vmul##q1##_lane##q2##_f##size (b, c, 0);			\
    }									\
  vst1##q1##_f##size (res, a);						\
}

#define BUILD_VARS(width, n_lanes, n_half_lanes)		\
TEST_VMUL ( ,  , width, n_half_lanes, n_half_lanes)		\
TEST_VMUL (q,  , width, n_lanes, n_half_lanes)			\
TEST_VMUL ( , q, width, n_half_lanes, n_lanes)			\
TEST_VMUL (q, q, width, n_lanes, n_lanes)

BUILD_VARS (32, 4, 2)
BUILD_VARS (64, 2, 1)

#define POOL2 {0.0, 1.0}
#define POOL4 {0.0, 1.0, 2.0, 3.0}
#define EMPTY2 {0.0, 0.0}
#define EMPTY4 {0.0, 0.0, 0.0, 0.0}

#define BUILD_TEST(size, lanes)					\
static void							\
test_f##size (void)						\
{								\
  int i;							\
  float##size##_t pool[lanes] = POOL##lanes;			\
  float##size##_t res[lanes] = EMPTY##lanes;			\
  float##size##_t res2[lanes] = EMPTY##lanes;			\
  float##size##_t res3[lanes] = EMPTY##lanes;			\
  float##size##_t res4[lanes] = EMPTY##lanes;			\
								\
  /* Avoid constant folding the multiplication.  */		\
  asm volatile ("" : : : "memory");				\
  test_vmul_lane_f##size (res, pool, pool);			\
  /* Avoid fusing multiplication and subtraction.  */		\
  asm volatile ("" : :"Q" (res) : "memory");			\
  for (i = 0; i < lanes / 2; i++)				\
    if (fabs (res[i] - pool[i]) > DELTA)			\
      abort ();							\
								\
  test_vmulq_lane_f##size (res2, pool, pool);			\
  /* Avoid fusing multiplication and subtraction.  */		\
  asm volatile ("" : :"Q" (res2) : "memory");			\
  for (i = 0; i < lanes; i++)					\
    if (fabs (res2[i] - pool[i]) > DELTA)			\
      abort ();							\
								\
  test_vmul_laneq_f##size (res3, pool, pool);			\
  /* Avoid fusing multiplication and subtraction.  */		\
  asm volatile ("" : :"Q" (res3) : "memory");			\
  for (i = 0; i < lanes / 2; i++)				\
    if (fabs (res3[i] - pool[i]) > DELTA)			\
      abort ();							\
								\
  test_vmulq_laneq_f##size (res4, pool, pool);			\
  /* Avoid fusing multiplication and subtraction.  */		\
  asm volatile ("" : :"Q" (res4) : "memory");			\
  for (i = 0; i < lanes; i++)					\
    if (fabs (res4[i] - pool[i]) > DELTA)			\
      abort ();							\
}

BUILD_TEST (32, 4)
BUILD_TEST (64, 2)

int
main (int argc, char **argv)
{
  test_f32 ();
  test_f64 ();
  return 0;
}

/* vmul_laneq_f32.
   vmul_lane_f32.  */
/* { dg-final { scan-assembler-times "fmul\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s, v\[0-9\]+\.s\\\[\[0-9\]+\\\]" 2 } } */

/* vmulq_lane_f32.
   vmulq_laneq_f32.  */
/* { dg-final { scan-assembler-times "fmul\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.s\\\[\[0-9\]+\\\]" 2 } } */

/* vmul_lane_f64.
   Vmul_laneq_f64. */
/* { dg-final { scan-assembler-times "fmul\\td\[0-9\]+, d\[0-9\]+, d\[0-9\]+" 1 { target aarch64_big_endian } } } */
/* { dg-final { scan-assembler-times "fmul\\td\[0-9\]+, d\[0-9\]+, d\[0-9\]+" 2 { target aarch64_little_endian } } } */

/* vmulq_lane_f64.
   vmulq_laneq_f64.  */
/* { dg-final { scan-assembler-times "fmul\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d, v\[0-9\]+\.d\\\[\[0-9\]+\\\]" 3 { target aarch64_big_endian } } } */
/* { dg-final { scan-assembler-times "fmul\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d, v\[0-9\]+\.d\\\[\[0-9\]+\\\]" 2 { target aarch64_little_endian } } } */


