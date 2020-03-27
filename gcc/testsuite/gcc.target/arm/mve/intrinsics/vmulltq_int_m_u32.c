/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint64x2_t
foo (uint64x2_t inactive, uint32x4_t a, uint32x4_t b, mve_pred16_t p)
{
  return vmulltq_int_m_u32 (inactive, a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vmulltt.u32"  }  } */

uint64x2_t
foo1 (uint64x2_t inactive, uint32x4_t a, uint32x4_t b, mve_pred16_t p)
{
  return vmulltq_int_m (inactive, a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vmulltt.u32"  }  } */
