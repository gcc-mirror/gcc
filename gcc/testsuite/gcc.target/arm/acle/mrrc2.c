/* Test the mrrc2 ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-options "-save-temps" } */
/* { dg-require-effective-target arm_coproc4_ok } */

#include "arm_acle.h"

uint64_t test_mrrc2 (void)
{
  return __arm_mrrc2 (10, 5, 3);
}

/* { dg-final { scan-assembler "mrrc2\tp10, #5, r\[r0-9\]*, r\[r0-9\]*, CR3\n" } } */
