/* Test the mcrr ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-options "-save-temps" } */
/* { dg-require-effective-target arm_coproc3_ok } */

#include "arm_acle.h"

void test_mcrr (uint64_t a)
{
  a += 77;
  __arm_mcrr (10, 5, a, 3);
}

/* { dg-final { scan-assembler "add\[^\n\]*#77\n" } } */
/* { dg-final { scan-assembler "mcrr\tp10, #5, r\[r0-9\]*, r\[r0-9\]*, CR3\n" } } */
