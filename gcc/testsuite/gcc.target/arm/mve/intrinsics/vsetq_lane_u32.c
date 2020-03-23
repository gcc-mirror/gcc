/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=soft" } {""} } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32x4_t
foo (uint32_t a, uint32x4_t b)
{
    return vsetq_lane_u32 (a, b, 0);
}

/* { dg-final { scan-assembler "vmov.32"  }  } */

