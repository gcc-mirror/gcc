/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O0" } */

#include "arm_mve.h"
int8x16_t a;
int16x8_t b;
int32x4_t c;
int64x2_t d;
uint8x16_t ua;
uint16x8_t ub;
uint32x4_t uc;
uint64x2_t ud;

void
foo ()
{
  a = vuninitializedq_s8 ();
  b = vuninitializedq_s16 ();
  c = vuninitializedq_s32 ();
  d = vuninitializedq_s64 ();
  ua = vuninitializedq_u8 ();
  ub = vuninitializedq_u16 ();
  uc = vuninitializedq_u32 ();
  ud = vuninitializedq_u64 ();
}

/* { dg-final { scan-assembler-times "vstrb.8" 16 } } */
