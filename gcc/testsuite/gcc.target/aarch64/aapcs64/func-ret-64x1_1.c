/* Test AAPCS64 layout.

  Test 64-bit singleton vector types which should be in FP/SIMD registers.  */

/* { dg-do run { target aarch64*-*-* } } */
/* { dg-additional-sources "abitest.S" } */

#ifndef IN_FRAMEWORK
#define TESTFILE "func-ret-64x1_1.c"
#include <arm_neon.h>
#include "abitest-2.h"
#else
FUNC_VAL_CHECK ( 0, float64x1_t, (float64x1_t) {123456.789}, D0, flat)
FUNC_VAL_CHECK ( 1, int64x1_t, (int64x1_t) {0xdeadbeefcafebabeLL}, D0, flat)
FUNC_VAL_CHECK ( 2, uint64x1_t, (uint64x1_t) {0xaaaabbbbccccddddULL}, D0, flat)
#endif

