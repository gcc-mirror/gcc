/* { dg-require-effective-target arm_hard_ok } */
/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-mfloat-abi=hard" } */
/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=soft" } {""} } */

#include "arm_mve.h"

float32x4_t
foo32 ()
{
  float32x4_t b = {10.0, 12.0, 14.0, 16.0};
  return b;
}

float16x8_t
foo16 ()
{
  float16x8_t b = {32.01};
  return b;
}

/* { dg-final { scan-assembler-times "vmov\\tq\[0-7\], q\[0-7\]" 2 } } */
/* { dg-final { scan-assembler-times "vstrw.32*" 1 } } */
/* { dg-final { scan-assembler-times "vstrh.16*" 1 } } */
/* { dg-final { scan-assembler-times "vldrw.32*" 1 } } */
/* { dg-final { scan-assembler-times "vldrh.16*" 1 } } */
/* { dg-final { scan-assembler-not "__ARM_undef" } } */
