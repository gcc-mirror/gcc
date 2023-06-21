/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
**foo11:
**     ...
**     movs	r0, #2
**     ...
*/
uint32_t
foo11 ()
{
  return uqshl (1, 1);
}

/*
**foo12:
**     ...
**	movs	r0, #2
**	movs	r1, #0
**     ...
*/
uint64_t
foo12 ()
{
  return uqshll (1, 1);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
