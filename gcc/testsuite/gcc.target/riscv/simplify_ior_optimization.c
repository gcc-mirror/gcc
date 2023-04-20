/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64 -O2" } */

#include "stdint-gcc.h"

uint8_t test_simplify_ior_scalar_case_0 (uint8_t a)
{
  return a | ~a;
}

uint16_t test_simplify_ior_scalar_case_1 (uint16_t a)
{
  return a | ~a;
}

uint32_t test_simplify_ior_scalar_case_2 (uint32_t a)
{
  return a | ~a;
}

uint64_t test_simplify_ior_scalar_case_3 (uint64_t a)
{
  return a | ~a;
}

int8_t test_simplify_ior_scalar_case_4 (int8_t a)
{
  return a | ~a;
}

int16_t test_simplify_ior_scalar_case_5 (int16_t a)
{
  return a | ~a;
}

int32_t test_simplify_ior_scalar_case_6 (int32_t a)
{
  return a | ~a;
}

int64_t test_simplify_ior_scalar_case_7 (int64_t a)
{
  return a | ~a;
}

/* { dg-final { scan-assembler-times {li\s+a[0-9]+,\s*-1} 6 } } */
/* { dg-final { scan-assembler-times {li\s+a[0-9]+,\s*255} 1 } } */
/* { dg-final { scan-assembler-times {li\s+a[0-9]+,\s*65536} 1 } } */
/* { dg-final { scan-assembler-not {or\s+a[0-9]+} } } */
/* { dg-final { scan-assembler-not {not\s+a[0-9]+} } } */
