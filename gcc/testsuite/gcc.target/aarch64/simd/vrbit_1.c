/* { dg-do run } */
/* { dg-options "-O2 --save-temps -fno-inline -fno-ipa-icf" } */

#include <arm_neon.h>

extern void abort (void);

uint64_t in1 = 0x0123456789abcdefULL;
uint64_t expected1 = 0x80c4a2e691d5b3f7ULL;

#define TEST8(BASETYPE, SUFFIX)						\
void test8_##SUFFIX ()							\
{									\
  BASETYPE##8x8_t out = vrbit_##SUFFIX (vcreate_##SUFFIX (in1));	\
  uint64_t res = vget_lane_u64 (vreinterpret_u64_##SUFFIX (out), 0);	\
  if (res != expected1) abort ();					\
}

uint64_t in2 = 0xdeadbeefcafebabeULL;
uint64_t expected2 = 0x7bb57df7537f5d7dULL;

#define TEST16(BASETYPE, SUFFIX)					\
void test16_##SUFFIX ()							\
{									\
  BASETYPE##8x16_t in = vcombine_##SUFFIX (vcreate_##SUFFIX (in1),	\
					   vcreate_##SUFFIX (in2));	\
  uint64x2_t res = vreinterpretq_u64_##SUFFIX (vrbitq_##SUFFIX (in));	\
  uint64_t res1 = vgetq_lane_u64 (res, 0);				\
  uint64_t res2 = vgetq_lane_u64 (res, 1);				\
  if (res1 != expected1 || res2 != expected2) abort ();			\
}

TEST8 (poly, p8);
TEST8 (int, s8);
TEST8 (uint, u8);

TEST16 (poly, p8);
TEST16 (int, s8);
TEST16 (uint, u8);

int
main (int argc, char **argv)
{
  test8_p8 ();
  test8_s8 ();
  test8_u8 ();
  test16_p8 ();
  test16_s8 ();
  test16_u8 ();
  return 0;
}

/* { dg-final { scan-assembler-times "rbit\[ \t\]+\[vV\]\[0-9\]+\.8\[bB\], ?\[vV\]\[0-9\]+\.8\[bB\]" 3 } } */
/* { dg-final { scan-assembler-times "rbit\[ \t\]+\[vV\]\[0-9\]+\.16\[bB\], ?\[vV\]\[0-9\]+\.16\[bB\]" 3 } } */

