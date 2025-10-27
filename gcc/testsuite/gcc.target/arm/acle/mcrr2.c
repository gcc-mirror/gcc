/* Test the mcrr2 ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-options "-save-temps" } */
/* { dg-require-effective-target arm_coproc4_ok } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include "arm_acle.h"
#if (__ARM_ARCH < 8 || !defined (__ARM_ARCH_ISA_ARM)) \
    && (__ARM_FEATURE_COPROC & 0x8) == 0
  #error "__ARM_FEATURE_COPROC does not have correct feature bits set"
#endif

void test_mcrr2 (uint64_t a)
{
  a += 77;
  __arm_mcrr2 (10, 5, a, 3);
}
/*
** test_mcrr2:
** ...
**	add.*#77
** ...
**	mcrr2	p10, #5, r[0-9]+, r[0-9]+, CR3
** ...
*/

void test_mcrr2_pr121464 (void)
{
  __arm_mcrr2 (3, 12, 49, 4);
}

/*
** test_mcrr2_pr121464:
** ...
**	mov.*#49
** ...
**	mcrr2	p3, #12, r[0-9]+, r[0-9]+, CR4
** ...
*/
