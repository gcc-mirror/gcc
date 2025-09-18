/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } }  */

/* LUTI4 (four registers, 8-bit)
   Variants are also available for: _u8
   svint8x4_t svluti4_zt_s8_x4 (uint64_t zt0, svuint8x2_t zn)
	      __arm_streaming __arm_in ("zt0");  */

#pragma GCC target "+sve2,+sme-lutv2"
#define SHARED_ZT0
#include "test_sme2_acle.h"

/*
** luti4_zt_u8_x4:
** luti4	{z0.b - z3.b}, zt0, {z0 - z1}
** ret
*/
PROTO (luti4_zt_u8_x4, svuint8x4_t, (svuint8x2_t z0))
{
  return svluti4_zt_u8_x4 (0, z0);
}

/*
** luti4_zt_s8_x4:
** luti4	{z0.b - z3.b}, zt0, {z0 - z1}
** ret
*/
PROTO (luti4_zt_s8_x4, svint8x4_t, (svuint8x2_t z0))
{
  return svluti4_zt_s8_x4 (0, z0);
}
