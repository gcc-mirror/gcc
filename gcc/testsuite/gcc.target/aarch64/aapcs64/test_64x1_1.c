/* Test AAPCS64 layout.

   Test 64-bit singleton vector types which should be in FP/SIMD registers.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_64x1_1.c"
#include <arm_neon.h>

#include "abitest.h"
#else
ARG (float64x1_t, (float64x1_t) {123456.789}, D0)
ARG (float64_t, 987654.321, D1)
ARG (float64x1_t, (float64x1_t) {13579.2468}, D2)
ARG (int64x1_t, (int64x1_t) {0xcafebabe0cabfaffLL}, D3)
ARG (uint64_t, 0xdeadbeefdeafbeeb, X0)
ARG (int64_t, 0x0123456789abcdef, X1)
LAST_ARG (uint64x1_t, (uint64x1_t) {0xaaaabbbbccccddddULL}, D4)
#endif
