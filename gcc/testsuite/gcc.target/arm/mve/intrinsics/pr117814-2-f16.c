/* Check that we can compile if the target does not support floating-point, but
   we use a pragma to enable FP support locally.  */

/* { dg-require-effective-target arm_v8_1m_mve_nofp_ok } */
/* { dg-add-options arm_v8_1m_mve_nofp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

#pragma GCC target ("arch=armv8.1-m.main+mve.fp")

/*
**foo:
**	...
**	vldrh.16	q[0-9]+, \[(?:ip|fp|r[0-9]+)\](?:	@.*|) 
**	...
*/
float16x8_t
foo (float16_t const *base)
{
  return vld1q_f16 (base);
}

#ifdef __cplusplus
}
#endif
