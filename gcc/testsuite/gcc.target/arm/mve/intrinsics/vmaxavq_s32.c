/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32_t
foo (uint32_t a, int32x4_t b)
{
  return vmaxavq_s32 (a, b);
}


uint32_t
foo1 (uint32_t a, int32x4_t b)
{
  return vmaxavq (a, b);
}


int32_t
foo2 (uint16_t a, int32x4_t b)
{
  return vmaxavq (a, b);
}

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
/* { dg-final { scan-assembler-times "vmaxav.s32" 3 } } */
