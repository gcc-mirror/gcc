/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
**foo:
**	...
**	vmov.32	q[0-9]+\[1\], (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
float32x4_t
foo (float32_t a, float32x4_t b)
{
  return vsetq_lane_f32 (a, b, 1);
}


/*
**foo1:
**	...
**	vmov.32	q[0-9]+\[1\], (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
float32x4_t
foo1 (float32_t a, float32x4_t b)
{
  return vsetq_lane (a, b, 1);
}

/*
**foo2:
**	...
**	vmov.32	q[0-9]+\[1\], (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
float32x4_t
foo2 (float32x4_t b)
{
  return vsetq_lane (1.1, b, 1);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */