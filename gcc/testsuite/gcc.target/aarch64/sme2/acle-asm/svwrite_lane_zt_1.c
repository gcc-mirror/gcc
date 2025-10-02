/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } }  */

/* MOVT (vector to table)
   Variants are also available for:
   [_s8], [_u16], [_s16], [_u32], [_s32], [_u64], [_s64]
   [_bf16], [_f16], [_f32], [_f64]
   void svwrite_lane_zt[_u8] (uint64_t zt0, svuint8_t zt, uint64_t idx)
	__arm_streaming __arm_out ("zt0");  */

#pragma GCC target "+sve2,+sme-lutv2"
#define SHARED_ZT0
#include "test_sme2_acle.h"

/*
** write_lane_zt_u8:
** movt	zt0 \[0, mul vl\], z0
** movt	zt0 \[1, mul vl\], z0
** movt	zt0 \[2, mul vl\], z0
** movt	zt0 \[3, mul vl\], z0
** ret
*/
PROTO (write_lane_zt_u8, void, (svuint8_t z0))
{
  svwrite_lane_zt_u8 (0, z0, 0);
  svwrite_lane_zt_u8 (0, z0, 1);
  svwrite_lane_zt_u8 (0, z0, 2);
  svwrite_lane_zt_u8 (0, z0, 3);
}

/*
** write_lane_zt_s8:
** movt	zt0 \[0, mul vl\], z0
** movt	zt0 \[1, mul vl\], z0
** movt	zt0 \[2, mul vl\], z0
** movt	zt0 \[3, mul vl\], z0
** ret
*/
PROTO (write_lane_zt_s8, void, (svint8_t z0))
{
  svwrite_lane_zt_s8 (0, z0, 0);
  svwrite_lane_zt_s8 (0, z0, 1);
  svwrite_lane_zt_s8 (0, z0, 2);
  svwrite_lane_zt_s8 (0, z0, 3);
}

/*
** write_lane_zt_u16:
** movt	zt0 \[0, mul vl\], z0
** movt	zt0 \[1, mul vl\], z0
** movt	zt0 \[2, mul vl\], z0
** movt	zt0 \[3, mul vl\], z0
** ret
*/
PROTO (write_lane_zt_u16, void, (svuint16_t z0))
{
  svwrite_lane_zt_u16 (0, z0, 0);
  svwrite_lane_zt_u16 (0, z0, 1);
  svwrite_lane_zt_u16 (0, z0, 2);
  svwrite_lane_zt_u16 (0, z0, 3);
}

/*
** write_lane_zt_s16:
** movt	zt0 \[0, mul vl\], z0
** movt	zt0 \[1, mul vl\], z0
** movt	zt0 \[2, mul vl\], z0
** movt	zt0 \[3, mul vl\], z0
** ret
*/
PROTO (write_lane_zt_s16, void, (svint16_t z0))
{
  svwrite_lane_zt_s16 (0, z0, 0);
  svwrite_lane_zt_s16 (0, z0, 1);
  svwrite_lane_zt_s16 (0, z0, 2);
  svwrite_lane_zt_s16 (0, z0, 3);
}

/*
** write_lane_zt_u32:
** movt	zt0 \[0, mul vl\], z0
** movt	zt0 \[1, mul vl\], z0
** movt	zt0 \[2, mul vl\], z0
** movt	zt0 \[3, mul vl\], z0
** ret
*/
PROTO (write_lane_zt_u32, void, (svuint32_t z0))
{
  svwrite_lane_zt_u32 (0, z0, 0);
  svwrite_lane_zt_u32 (0, z0, 1);
  svwrite_lane_zt_u32 (0, z0, 2);
  svwrite_lane_zt_u32 (0, z0, 3);
}

/*
** write_lane_zt_s32:
** movt	zt0 \[0, mul vl\], z0
** movt	zt0 \[1, mul vl\], z0
** movt	zt0 \[2, mul vl\], z0
** movt	zt0 \[3, mul vl\], z0
** ret
*/
PROTO (write_lane_zt_s32, void, (svint32_t z0))
{
  svwrite_lane_zt_s32 (0, z0, 0);
  svwrite_lane_zt_s32 (0, z0, 1);
  svwrite_lane_zt_s32 (0, z0, 2);
  svwrite_lane_zt_s32 (0, z0, 3);
}

/*
** write_lane_zt_u64:
** movt	zt0 \[0, mul vl\], z0
** movt	zt0 \[1, mul vl\], z0
** movt	zt0 \[2, mul vl\], z0
** movt	zt0 \[3, mul vl\], z0
** ret
*/
PROTO (write_lane_zt_u64, void, (svuint64_t z0))
{
  svwrite_lane_zt_u64 (0, z0, 0);
  svwrite_lane_zt_u64 (0, z0, 1);
  svwrite_lane_zt_u64 (0, z0, 2);
  svwrite_lane_zt_u64 (0, z0, 3);
}

/*
** write_lane_zt_s64:
** movt	zt0 \[0, mul vl\], z0
** movt	zt0 \[1, mul vl\], z0
** movt	zt0 \[2, mul vl\], z0
** movt	zt0 \[3, mul vl\], z0
** ret
*/
PROTO (write_lane_zt_s64, void, (svint64_t z0))
{
  svwrite_lane_zt_s64 (0, z0, 0);
  svwrite_lane_zt_s64 (0, z0, 1);
  svwrite_lane_zt_s64 (0, z0, 2);
  svwrite_lane_zt_s64 (0, z0, 3);
}

/*
** write_lane_zt_bf16:
** movt	zt0 \[0, mul vl\], z0
** movt	zt0 \[1, mul vl\], z0
** movt	zt0 \[2, mul vl\], z0
** movt	zt0 \[3, mul vl\], z0
** ret
*/
PROTO (write_lane_zt_bf16, void, (svbfloat16_t z0))
{
  svwrite_lane_zt_bf16 (0, z0, 0);
  svwrite_lane_zt_bf16 (0, z0, 1);
  svwrite_lane_zt_bf16 (0, z0, 2);
  svwrite_lane_zt_bf16 (0, z0, 3);
}

/*
** write_lane_zt_f16:
** movt	zt0 \[0, mul vl\], z0
** movt	zt0 \[1, mul vl\], z0
** movt	zt0 \[2, mul vl\], z0
** movt	zt0 \[3, mul vl\], z0
** ret
*/
PROTO (write_lane_zt_f16, void, (svfloat16_t z0))
{
  svwrite_lane_zt_f16 (0, z0, 0);
  svwrite_lane_zt_f16 (0, z0, 1);
  svwrite_lane_zt_f16 (0, z0, 2);
  svwrite_lane_zt_f16 (0, z0, 3);
}

/*
** write_lane_zt_f32:
** movt	zt0 \[0, mul vl\], z0
** movt	zt0 \[1, mul vl\], z0
** movt	zt0 \[2, mul vl\], z0
** movt	zt0 \[3, mul vl\], z0
** ret
*/
PROTO (write_lane_zt_f32, void, (svfloat32_t z0))
{
  svwrite_lane_zt_f32 (0, z0, 0);
  svwrite_lane_zt_f32 (0, z0, 1);
  svwrite_lane_zt_f32 (0, z0, 2);
  svwrite_lane_zt_f32 (0, z0, 3);
}

/*
** write_lane_zt_f64:
** movt	zt0 \[0, mul vl\], z0
** movt	zt0 \[1, mul vl\], z0
** movt	zt0 \[2, mul vl\], z0
** movt	zt0 \[3, mul vl\], z0
** ret
*/
PROTO (write_lane_zt_f64, void, (svfloat64_t z0))
{
  svwrite_lane_zt_f64 (0, z0, 0);
  svwrite_lane_zt_f64 (0, z0, 1);
  svwrite_lane_zt_f64 (0, z0, 2);
  svwrite_lane_zt_f64 (0, z0, 3);
}
