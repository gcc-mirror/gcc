/* Test the mrc ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-options "-save-temps" } */
/* { dg-require-effective-target arm_coproc1_ok } */

#include "arm_acle.h"

uint32_t test_mrc (void)
{
  return __arm_mrc (10, 0, 0, 15, 3);
}

/* { dg-final { scan-assembler "mrc\tp10, #0, r\[r0-9\]*, CR0, CR15, #3\n" } } */
