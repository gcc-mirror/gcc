/* { dg-do compile } */

#include <arm_sme.h>

#pragma GCC target ("arch=armv9-a+sme2+nosme-i16i64")

void
f1 (svint32x2_t s32x2, svuint32x2_t u32x2,
    svint64x2_t s64x2, svuint64x2_t u64x2)
  __arm_streaming __arm_inout("za")
{
  svadd_za64_vg1x2 (1, s32x2); /* { dg-error {'svadd_za64_vg1x2' has no form that takes 'svint32x2_t' arguments} } */
  svadd_za64_vg1x2 (1, u32x2); /* { dg-error {'svadd_za64_vg1x2' has no form that takes 'svuint32x2_t' arguments} } */
  svadd_za64_vg1x2 (1, s64x2); /* { dg-error {ACLE function 'svadd_za64_s64_vg1x2' requires ISA extension 'sme-i16i64'} } */
  svadd_za64_vg1x2 (1, u64x2);
}
