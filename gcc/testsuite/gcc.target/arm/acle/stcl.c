/* Test the stcl ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-options "-save-temps" } */
/* { dg-require-effective-target arm_coproc1_ok } */

#include "arm_acle.h"

extern void * p;

void test_stcl (void)
{
  __arm_stcl (14, 10, p + 4);
  __arm_stcl (10, 10, p + 1024);
}

/* { dg-final { scan-assembler "stcl\tp14, CR10, \[r\[0-9\]+" } } */
/* { dg-final { scan-assembler "stcl\tp10, CR10, \[r\[0-9\]+\]\n" } } */
