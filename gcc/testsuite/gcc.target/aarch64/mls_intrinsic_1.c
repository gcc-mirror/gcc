/* { dg-do run } */
/* { dg-options "-O3 --save-temps" } */

#include <arm_neon.h>

extern void abort (void);

#define MAPs(size, xx) int##size##xx##_t
#define MAPu(size, xx) uint##size##xx##_t


#define TEST_VMLS(q, su, size, in1_lanes, in2_lanes)		\
static void							\
__attribute__((noipa,noinline))					\
test_vmlsq_lane##q##_##su##size (MAP##su (size, ) * res,	\
				 const MAP##su(size, ) *in1,	\
				 const MAP##su(size, ) *in2)	\
{								\
  MAP##su (size, x##in1_lanes) a = vld1q_##su##size (res);	\
  MAP##su (size, x##in1_lanes) b = vld1q_##su##size (in1);	\
  MAP##su (size, x##in2_lanes) c = vld1##q##_##su##size (in2);	\
  a = vmlsq_lane##q##_##su##size (a, b, c, 1);			\
  vst1q_##su##size (res, a);					\
}

#define BUILD_VARS(width, n_lanes, n_half_lanes)		\
TEST_VMLS (, s, width, n_lanes, n_half_lanes)			\
TEST_VMLS (q, s, width, n_lanes, n_lanes)			\
TEST_VMLS (, u, width, n_lanes, n_half_lanes)			\
TEST_VMLS (q, u, width, n_lanes, n_lanes)			\

BUILD_VARS (32, 4, 2)
BUILD_VARS (16, 8, 4)

#define MAP_OPs +
#define MAP_OPu -

#define POOL4 {0, 1, 2, 3}
#define POOL8 {0, 1, 2, 3, 4, 5, 6, 7}
#define EMPTY4s {0, 0, 0, 0}
#define EMPTY8s {0, 0, 0, 0, 0, 0, 0, 0}
#define EMPTY4u {0, 2, 4, 6}
#define EMPTY8u {0, 2, 4, 6, 8, 10, 12, 14}

#define BUILD_TEST(su, size, lanes)				\
static void							\
test_##su##size (void)						\
{								\
  int i;							\
  MAP##su (size,) pool[lanes] = POOL##lanes;			\
  MAP##su (size,) res[lanes] = EMPTY##lanes##su;		\
  MAP##su (size,) res2[lanes] = EMPTY##lanes##su;		\
								\
  /* Forecfully avoid optimization.  */				\
  asm volatile ("" : : : "memory");				\
  test_vmlsq_lane_##su##size (res, pool, pool);			\
  for (i = 0; i < lanes; i++)					\
    if (res[i] MAP_OP##su pool[i] != 0)				\
      abort ();							\
								\
  /* Forecfully avoid optimization.  */				\
  asm volatile ("" : : : "memory");				\
  test_vmlsq_laneq_##su##size (res2, pool, pool);		\
  for (i = 0; i < lanes; i++)					\
    if (res2[i] MAP_OP##su pool[i] != 0)			\
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

/* { dg-final { scan-assembler-times "mls\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.s\\\[\[0-9\]+\\\]" 4 } } */
/* { dg-final { scan-assembler-times "mls\\tv\[0-9\]+\.8h, v\[0-9\]+\.8h, v\[0-9\]+\.h\\\[\[0-9\]+\\\]" 4 } } */

