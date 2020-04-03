/* { dg-do assemble} */
/* { dg-require-effective-target arm_v8_2a_i8mm_ok } */
/* { dg-additional-options "-march=armv8.2-a+i8mm" } */

#include "arm_neon.h"

int32x4_t
test_vmmlaq_s32 (int32x4_t r, int8x16_t a, int8x16_t b)
{
  return vmmlaq_s32 (r, a, b);
}

uint32x4_t
test_vmmlaq_u32 (uint32x4_t r, uint8x16_t a, uint8x16_t b)
{
  return vmmlaq_u32 (r, a, b);
}

int32x4_t
test_vusmmlaq_s32 (int32x4_t r, uint8x16_t a, int8x16_t b)
{
  return vusmmlaq_s32 (r, a, b);
}

/* { dg-final { scan-assembler-times {\tsmmla\tv[0-9]+.4s, v[0-9]+.16b, v[0-9]+.16b} 1 } } */
/* { dg-final { scan-assembler-times {\tummla\tv[0-9]+.4s, v[0-9]+.16b, v[0-9]+.16b} 1 } } */
/* { dg-final { scan-assembler-times {\tusmmla\tv[0-9]+.4s, v[0-9]+.16b, v[0-9]+.16b} 1 } } */
