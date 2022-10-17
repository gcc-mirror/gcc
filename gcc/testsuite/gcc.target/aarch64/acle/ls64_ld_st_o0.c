/* { dg-do compile } */
/* { dg-options "-march=armv8-a+ls64 -O0" } */

#ifndef __ARM_FEATURE_LS64
#error "__ARM_FEATURE_LS64 is not defined but should be!"
#endif

#include <arm_acle.h>

/* Make sure no issues when compile with -O0.  */

data512_t
func1 (const void * addr) {
  return __arm_ld64b (addr);
}

void
func2 (void *addr, data512_t value) {
    __arm_st64b (addr, value);
}

uint64_t
func3 (void *addr, data512_t value) {
    return  __arm_st64bv (addr, value);
}

uint64_t
func4 (void *addr, data512_t value) {
    return __arm_st64bv0 (addr, value);
}
