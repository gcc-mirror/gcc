/* Test that __arm_[r,w]sr intrinsics aren't gated (by default).  */

/* { dg-do compile } */
/* { dg-options "-march=armv8-a" } */

#include <arm_acle.h>

uint64_t
foo (uint64_t a)
{
  __arm_wsr64 ("zcr_el1", a);
  return __arm_rsr64 ("smcr_el1");
}
