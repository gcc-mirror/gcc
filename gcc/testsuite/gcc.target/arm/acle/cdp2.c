/* Test the cdp2 ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-options "-save-temps" } */
/* { dg-require-effective-target arm_coproc2_ok } */

#include "arm_acle.h"

void test_cdp2 (void)
{
  __arm_cdp2 (10, 4, 3, 2, 1, 0);
}

/* { dg-final { scan-assembler "cdp2\tp10, #4, CR3, CR2, CR1, #0\n" } } */
