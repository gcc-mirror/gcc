/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int8x16_t a, aa;
int16x8_t b, bb;
int32x4_t c, cc;
int64x2_t d, dd;
uint8x16_t ua, uaa;
uint16x8_t ub, ubb;
uint32x4_t uc, ucc;
uint64x2_t ud, udd;

void
foo ()
{
  a = vuninitializedq (aa);
  b = vuninitializedq (bb);
  c = vuninitializedq (cc);
  d = vuninitializedq (dd);
  ua = vuninitializedq (uaa);
  ub = vuninitializedq (ubb);
  uc = vuninitializedq (ucc);
  ud = vuninitializedq (udd);
}

/* { dg-final { scan-assembler-times "vstrb.8" 2 } } */
/* { dg-final { scan-assembler-times "vstrh.16" 2 } } */
/* { dg-final { scan-assembler-times "vstrw.32" 2 } } */
/* { dg-final { scan-assembler-times "vstr.64" 2 } } */
/* { dg-final { scan-assembler-not "__ARM_undef" } } */
