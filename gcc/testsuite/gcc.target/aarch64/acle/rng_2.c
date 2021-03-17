/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8.5-a+rng" } */

#include <arm_acle.h>

int test_rndr (uint64_t *addr)
{
  return  __rndr (addr);
}

/* { dg-final { scan-assembler-times {cset\t...?, eq} 1 } } */

