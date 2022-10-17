/* { dg-do compile } */
/* { dg-options "-march=armv8-a+ls64 -O2" } */

#ifndef __ARM_FEATURE_LS64
#error "__ARM_FEATURE_LS64 is not defined but should be!"
#endif

#include <arm_acle.h>

void
func(void *addr, data512_t value) {
    __arm_st64bv (addr, value);
}

/* { dg-final { scan-assembler-times {st64bv\t} 1 } } */
