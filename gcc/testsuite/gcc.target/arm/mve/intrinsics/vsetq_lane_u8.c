/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=soft" } {""} } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint8x16_t
foo (uint8_t a, uint8x16_t b)
{
    return vsetq_lane_u8 (a, b, 0);
}

/* { dg-final { scan-assembler "vmov.8"  }  } */

