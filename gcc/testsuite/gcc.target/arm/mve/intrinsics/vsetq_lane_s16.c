/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=soft" } {""} } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int16x8_t
foo (int16_t a, int16x8_t b)
{
    return vsetq_lane_s16 (a, b, 0);
}

/* { dg-final { scan-assembler "vmov.16"  }  } */

