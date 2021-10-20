/* { dg-do run } */
/* { dg-options "-O3 --save-temps" } */

#include <arm_neon.h>

extern void abort (void);

#define MAPs(size, xx) int##size##xx##_t
#define MAPu(size, xx) uint##size##xx##_t


#define TEST_VMUL(q, su, size, in1_lanes, in2_lanes)		\
static void							\
__attribute__((noipa,noinline))					\
test_vmulq_lane##q##_##su##size (MAP##su (size, ) * res,	\
				 const MAP##su(size, ) *in1,	\
				 const MAP##su(size, ) *in2)	\
{								\
  MAP##su (size, x##in1_lanes) a = vld1q_##su##size (in1);	\
  MAP##su (size, x##in2_lanes) b = vld1##q##_##su##size (in2);	\
  a = vmulq_lane##q##_##su##size (a, b, 1);			\
  vst1q_##su##size (res, a);					\
}

#define BUILD_VARS(width, n_lanes, n_half_lanes)		\
TEST_VMUL (, s, width, n_lanes, n_half_lanes)			\
TEST_VMUL (q, s, width, n_lanes, n_lanes)			\
TEST_VMUL (, u, width, n_lanes, n_half_lanes)			\
TEST_VMUL (q, u, width, n_lanes, n_lanes)			\

BUILD_VARS (32, 4, 2)
BUILD_VARS (16, 8, 4)

#define POOL4 {0, 1, 2, 3}
#define POOL8 {0, 1, 2, 3, 4, 5, 6, 7}
#define EMPTY4 {0, 0, 0, 0}
#define EMPTY8 {0, 0, 0, 0, 0, 0, 0, 0}

#define BUILD_TEST(su, size, lanes)				\
static void							\
test_##su##size (void)						\
{								\
  int i;							\
  MAP##su (size,) pool[lanes] = POOL##lanes;			\
  MAP##su (size,) res[lanes] = EMPTY##lanes;			\
  MAP##su (size,) res2[lanes] = EMPTY##lanes;			\
								\
  /* Forecfully avoid optimization.  */				\
  asm volatile ("" : : : "memory");				\
  test_vmulq_lane_##su##size (res, pool, pool);			\
  for (i = 0; i < lanes; i++)					\
    if (res[i] != pool[i])					\
      abort ();							\
								\
  /* Forecfully avoid optimization.  */				\
  asm volatile ("" : : : "memory");				\
  test_vmulq_laneq_##su##size (res2, pool, pool);		\
  for (i = 0; i < lanes; i++)					\
    if (res2[i] != pool[i])					\
      abort ();							\
}

#undef BUILD_VARS
#define BUILD_VARS(size, lanes)					\
BUILD_TEST (s, size, lanes)					\
BUILD_TEST (u, size, lanes)

BUILD_VARS (32, 4)
BUILD_VARS (16, 8)

int
main (int argc, char **argv)
{
  test_s32 ();
  test_u32 ();
  test_s16 ();
  test_u16 ();
  return 0;
}

/* { dg-final { scan-assembler-times "mul\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.s\\\[\[0-9\]+\\\]" 4 } } */
/* { dg-final { scan-assembler-times "mul\\tv\[0-9\]+\.8h, v\[0-9\]+\.8h, v\[0-9\]+\.h\\\[\[0-9\]+\\\]" 4 } } */

