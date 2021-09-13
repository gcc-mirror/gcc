/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=soft" } {""} } */
/* { dg-require-effective-target arm_hard_ok } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-mfloat-abi=hard -O2" } */

#include "arm_mve.h"

int64x2_t
foo (int64_t a, int64x2_t b)
{
    return vsetq_lane_s64 (a, b, 0);
}

/* { dg-final { scan-assembler {vmov\td0, r[1-9]*[0-9], r[1-9]*[0-9]}  }  } */

