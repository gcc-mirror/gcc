/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

float16x8_t
foo (float16_t const * base, uint16x8_t offset)
{
  return vldrhq_gather_offset_f16 (base, offset);
}

/* { dg-final { scan-assembler "vldrh.f16"  }  } */

float16x8_t
foo1 (float16_t const * base, uint16x8_t offset)
{
  return vldrhq_gather_offset (base, offset);
}

/* { dg-final { scan-assembler "vldrh.f16"  }  } */
