/* { dg-do assemble } */
/* { dg-options "-O2 -save-temps" } */

#include <arm_neon.h>

uint32x4_t foo (int32x4_t a)
{
  int32x4_t cst = vdupq_n_s32 (255);
  int32x4_t zero = vdupq_n_s32 (0);
  return vceqq_s32 (vbicq_s32 (a, cst), zero);
}

/* { dg-final { scan-assembler-not {\tbic\t} } } */
