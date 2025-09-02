/* Test the mrrc2 ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-options "-save-temps" } */
/* { dg-require-effective-target arm_coproc4_ok } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include "arm_acle.h"
#if (__ARM_ARCH < 8 || !defined (__ARM_ARCH_ISA_ARM)) \
    && (__ARM_FEATURE_COPROC & 0x8) == 0
  #error "__ARM_FEATURE_COPROC does not have correct feature bits set"
#endif

uint64_t test_mrrc2 (void)
{
  return __arm_mrrc2 (10, 5, 3);
}
/*
** test_mrrc2:
** ...
**	mrrc2	p10, #5, r[0-9]+, r[0-9]+, CR3
** ...
*/

uint64_t test_mrrc2_pr121464 (void)
{
  return __arm_mrrc2 (0, 15, 10);
}
/*
** test_mrrc2_pr121464:
** ...
**	mrrc2	p0, #15, r[0-9]+, r[0-9]+, CR10
** ...
*/
