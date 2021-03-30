/* { dg-do compile } */
/* { dg-options "-march=armv8-a+nosimd" } */

#include <stdint.h>

uint64_t test_double_to_uint64(double x) {
  return (uint64_t)x;
}

int64_t test_double_to_int64(double x) {
  return (int64_t)x;
}

uint32_t test_float_to_uint32(float x) {
  return (uint32_t)x;
}

int32_t test_float_to_int32(float x) {
  return (int32_t)x;
}

/* { dg-final { scan-assembler-not {\tfcvtz[su]\td[0-9]*, d[0-9]*} } } */
/* { dg-final { scan-assembler-not {\tfcvtz[su]\ts[0-9]*, s[0-9]*} } } */
