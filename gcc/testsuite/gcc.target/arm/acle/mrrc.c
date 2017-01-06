/* Test the mrrc ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-options "-save-temps" } */
/* { dg-require-effective-target arm_coproc3_ok } */

#include "arm_acle.h"

uint64_t test_mrrc (void)
{
  return __arm_mrrc (10, 5, 3);
}

/* { dg-final { scan-assembler "mrrc\tp10, #5, r\[r0-9\]*, r\[r0-9\]*, CR3\n" } } */
