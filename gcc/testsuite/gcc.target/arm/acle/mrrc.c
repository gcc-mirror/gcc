/* Test the mrrc ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-options "-save-temps" } */
/* { dg-require-effective-target arm_coproc3_ok } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include "arm_acle.h"
#if (__ARM_ARCH < 8 || !defined (__ARM_ARCH_ISA_ARM)) \
    && (__ARM_FEATURE_COPROC & 0x4) == 0
  #error "__ARM_FEATURE_COPROC does not have correct feature bits set"
#endif

uint64_t test_mrrc (void)
{
  return __arm_mrrc (10, 5, 3);
}

/*
** test_mrrc:
** ...
**	mrrc	p10, #5, r[0-9]+, r[0-9]+, CR3
** ...
*/

uint64_t test_mrrc_pr121464 (void)
{
  return __arm_mrrc (15, 9, 7);
}

/*
** test_mrrc_pr121464:
** ...
**	mrrc	p15, #9, r[0-9]+, r[0-9]+, CR7
** ...
*/
