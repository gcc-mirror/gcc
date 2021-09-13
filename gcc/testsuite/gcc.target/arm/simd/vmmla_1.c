/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_2a_i8mm_ok } */
/* { dg-options "-save-temps -O2 -mfpu=auto" } */
/* { dg-add-options arm_v8_2a_i8mm } */

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

/* { dg-final { scan-assembler-times {\tvsmmla.s8\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {\tvummla.u8\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {\tvusmmla.s8\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } } */
