/* { dg-do compile } */
/* { dg-require-effective-target arm_crypto_ok } */
/* { dg-add-options arm_crypto } */
/* { dg-additional-options "-O3" } */

#include "arm_neon.h"

uint32_t foo (void)

{
  uint32_t val = 0xdeadbeef;
  return vsha1h_u32 (val);
}

#define GET_LANE(lane)						\
	uint32_t foo_lane##lane (uint32x4_t val)		\
	{							\
	    return vsha1h_u32 (vgetq_lane_u32 (val, lane));	\
	}

#define TEST_SHA1H_VEC_SELECT(FUNC)				\
	FUNC (0)						\
	FUNC (1)						\
	FUNC (2)						\
	FUNC (3)						\

TEST_SHA1H_VEC_SELECT (GET_LANE)

/* { dg-final { scan-assembler-times {sha1h.32\tq[0-9]+, q[0-9]+} 5 } } */
/* { dg-final { scan-assembler-times {vdup.32\tq[0-9]+, r[0-9]+} 3 } } */
/* { dg-final { scan-assembler-times {vmov.32\tr[0-9]+, d[0-9]+\[[0-9]+\]+} 8 } } */
