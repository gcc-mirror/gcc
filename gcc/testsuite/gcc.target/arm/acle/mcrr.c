/* Test the mcrr ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-options "-save-temps" } */
/* { dg-require-effective-target arm_coproc3_ok } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include "arm_acle.h"
#if (__ARM_ARCH < 8 || !defined (__ARM_ARCH_ISA_ARM)) \
    && (__ARM_FEATURE_COPROC & 0x4) == 0
  #error "__ARM_FEATURE_COPROC does not have correct feature bits set"
#endif

void test_mcrr (uint64_t a)
{
  a += 77;
  __arm_mcrr (10, 5, a, 3);
}
/*
** test_mcrr:
** ...
**	add.*#77
** ...
**	mcrr	p10, #5, r[0-9]+, r[0-9]+, CR3
** ...
*/

void test_mcrr_pr121464 (uint64_t a)
{
  __arm_mcrr (7, 11, a, 0);
}
/*
** test_mcrr_pr121464:
** ...
**	mcrr	p7, #11, r[0-9]+, r[0-9]+, CR0
** ...
*/
