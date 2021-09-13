/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=soft" } {""} } */
/* { dg-require-effective-target arm_hard_ok } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-mfloat-abi=hard -O2" } */

#include "arm_mve.h"

uint64_t
foo (uint64x2_t a)
{
  return vgetq_lane_u64 (a, 0);
}

/* { dg-final { scan-assembler {vmov\tr0, r1, d0}  }  } */

uint64_t
foo1 (uint64x2_t a)
{
  return vgetq_lane (a, 0);
}

/* { dg-final { scan-assembler {vmov\tr0, r1, d0}  }  } */
