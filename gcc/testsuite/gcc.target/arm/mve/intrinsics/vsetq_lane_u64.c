/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=soft" } {""} } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint64x2_t
foo (uint64_t a, uint64x2_t b)
{
    return vsetq_lane_u64 (a, b, 0);
}

/* { dg-final { scan-assembler {vmov\td0, r[1-9]*[0-9], r[1-9]*[0-9]}  }  } */

