/* Test the mcr ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-options "-save-temps" } */
/* { dg-require-effective-target arm_coproc1_ok } */

#include "arm_acle.h"

void test_mcr (uint32_t a)
{
  a += 77;
  __arm_mcr (10, 5, a, 3, 4, 7);
}

/* { dg-final { scan-assembler "add\[^\n\]*#77\n" } } */
/* { dg-final { scan-assembler "mcr\tp10, #5, r\[r0-9\]*, CR3, CR4, #7\n" } } */
