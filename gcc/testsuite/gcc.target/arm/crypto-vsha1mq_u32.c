/* { dg-do compile } */
/* { dg-require-effective-target arm_crypto_ok } */
/* { dg-add-options arm_crypto } */
/* { dg-additional-options "-O3" } */

#include "arm_neon.h"

uint32_t foo (void)

{
  uint32_t hash = 0xdeadbeef;
  uint32x4_t a = {0, 1, 2, 3};
  uint32x4_t b = {3, 2, 1, 0};

  uint32x4_t res = vsha1mq_u32 (a, hash, b);
  return res[0];
}

#define GET_LANE(lane)							     \
	uint32x4_t foo_lane##lane (uint32x4_t val,uint32x4_t a, uint32x4_t b)\
	{								     \
	    return vsha1mq_u32 (a, vgetq_lane_u32 (val, lane), b);	     \
	}

#define TEST_SHA1M_VEC_SELECT(FUNC)				\
	FUNC (0)						\
	FUNC (1)						\
	FUNC (2)						\
	FUNC (3)						\

TEST_SHA1M_VEC_SELECT (GET_LANE)

/* { dg-final { scan-assembler-times {sha1m.32\tq[0-9]+, q[0-9]+} 5 } } */
/* { dg-final { scan-assembler-times {vdup.32\tq[0-9]+, r[0-9]+} 3 } } */
/* { dg-final { scan-assembler-times {vmov.32\tr[0-9]+, d[0-9]+\[[0-9]+\]+} 4 } } */
