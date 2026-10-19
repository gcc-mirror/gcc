/* PR rtl-optimization/127483 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_neon.h>

/* Pairwise additions of the two registers of a structure load.  */

uint8x8_t
addp_u8 (const uint8_t *p)
{
  uint8x8x2_t v = vld2_u8 (p);
  return vpadd_u8 (v.val[0], v.val[1]);
}

uint8x8_t
addp_u8_rev (const uint8_t *p)
{
  uint8x8x2_t v = vld2_u8 (p);
  return vpadd_u8 (v.val[1], v.val[0]);
}

int32x2_t
addp_s32 (const int32_t *p)
{
  int32x2x2_t v = vld2_s32 (p);
  return vpadd_s32 (v.val[0], v.val[1]);
}
