/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** reinterpret_u16_bf16_tied1:
**	ret
*/
TEST_DUAL_Z_REV (reinterpret_u16_bf16_tied1, svuint16_t, svbfloat16_t,
		 z0_res = svreinterpret_u16_bf16 (z0),
		 z0_res = svreinterpret_u16 (z0))

/*
** reinterpret_u16_bf16_untied:
**	mov	z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (reinterpret_u16_bf16_untied, svuint16_t, svbfloat16_t,
	     z0 = svreinterpret_u16_bf16 (z4),
	     z0 = svreinterpret_u16 (z4))

/*
** reinterpret_u16_f16_tied1:
**	ret
*/
TEST_DUAL_Z_REV (reinterpret_u16_f16_tied1, svuint16_t, svfloat16_t,
		 z0_res = svreinterpret_u16_f16 (z0),
		 z0_res = svreinterpret_u16 (z0))

/*
** reinterpret_u16_f16_untied:
**	mov	z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (reinterpret_u16_f16_untied, svuint16_t, svfloat16_t,
	     z0 = svreinterpret_u16_f16 (z4),
	     z0 = svreinterpret_u16 (z4))

/*
** reinterpret_u16_f32_tied1:
**	ret
*/
TEST_DUAL_Z_REV (reinterpret_u16_f32_tied1, svuint16_t, svfloat32_t,
		 z0_res = svreinterpret_u16_f32 (z0),
		 z0_res = svreinterpret_u16 (z0))

/*
** reinterpret_u16_f32_untied:
**	mov	z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (reinterpret_u16_f32_untied, svuint16_t, svfloat32_t,
	     z0 = svreinterpret_u16_f32 (z4),
	     z0 = svreinterpret_u16 (z4))

/*
** reinterpret_u16_f64_tied1:
**	ret
*/
TEST_DUAL_Z_REV (reinterpret_u16_f64_tied1, svuint16_t, svfloat64_t,
		 z0_res = svreinterpret_u16_f64 (z0),
		 z0_res = svreinterpret_u16 (z0))

/*
** reinterpret_u16_f64_untied:
**	mov	z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (reinterpret_u16_f64_untied, svuint16_t, svfloat64_t,
	     z0 = svreinterpret_u16_f64 (z4),
	     z0 = svreinterpret_u16 (z4))

/*
** reinterpret_u16_s8_tied1:
**	ret
*/
TEST_DUAL_Z_REV (reinterpret_u16_s8_tied1, svuint16_t, svint8_t,
		 z0_res = svreinterpret_u16_s8 (z0),
		 z0_res = svreinterpret_u16 (z0))

/*
** reinterpret_u16_s8_untied:
**	mov	z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (reinterpret_u16_s8_untied, svuint16_t, svint8_t,
	     z0 = svreinterpret_u16_s8 (z4),
	     z0 = svreinterpret_u16 (z4))

/*
** reinterpret_u16_s16_tied1:
**	ret
*/
TEST_DUAL_Z_REV (reinterpret_u16_s16_tied1, svuint16_t, svint16_t,
		 z0_res = svreinterpret_u16_s16 (z0),
		 z0_res = svreinterpret_u16 (z0))

/*
** reinterpret_u16_s16_untied:
**	mov	z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (reinterpret_u16_s16_untied, svuint16_t, svint16_t,
	     z0 = svreinterpret_u16_s16 (z4),
	     z0 = svreinterpret_u16 (z4))

/*
** reinterpret_u16_s32_tied1:
**	ret
*/
TEST_DUAL_Z_REV (reinterpret_u16_s32_tied1, svuint16_t, svint32_t,
		 z0_res = svreinterpret_u16_s32 (z0),
		 z0_res = svreinterpret_u16 (z0))

/*
** reinterpret_u16_s32_untied:
**	mov	z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (reinterpret_u16_s32_untied, svuint16_t, svint32_t,
	     z0 = svreinterpret_u16_s32 (z4),
	     z0 = svreinterpret_u16 (z4))

/*
** reinterpret_u16_s64_tied1:
**	ret
*/
TEST_DUAL_Z_REV (reinterpret_u16_s64_tied1, svuint16_t, svint64_t,
		 z0_res = svreinterpret_u16_s64 (z0),
		 z0_res = svreinterpret_u16 (z0))

/*
** reinterpret_u16_s64_untied:
**	mov	z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (reinterpret_u16_s64_untied, svuint16_t, svint64_t,
	     z0 = svreinterpret_u16_s64 (z4),
	     z0 = svreinterpret_u16 (z4))

/*
** reinterpret_u16_u8_tied1:
**	ret
*/
TEST_DUAL_Z_REV (reinterpret_u16_u8_tied1, svuint16_t, svuint8_t,
		 z0_res = svreinterpret_u16_u8 (z0),
		 z0_res = svreinterpret_u16 (z0))

/*
** reinterpret_u16_u8_untied:
**	mov	z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (reinterpret_u16_u8_untied, svuint16_t, svuint8_t,
	     z0 = svreinterpret_u16_u8 (z4),
	     z0 = svreinterpret_u16 (z4))

/*
** reinterpret_u16_u16_tied1:
**	ret
*/
TEST_DUAL_Z_REV (reinterpret_u16_u16_tied1, svuint16_t, svuint16_t,
		 z0_res = svreinterpret_u16_u16 (z0),
		 z0_res = svreinterpret_u16 (z0))

/*
** reinterpret_u16_u16_untied:
**	mov	z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (reinterpret_u16_u16_untied, svuint16_t, svuint16_t,
	     z0 = svreinterpret_u16_u16 (z4),
	     z0 = svreinterpret_u16 (z4))

/*
** reinterpret_u16_u32_tied1:
**	ret
*/
TEST_DUAL_Z_REV (reinterpret_u16_u32_tied1, svuint16_t, svuint32_t,
		 z0_res = svreinterpret_u16_u32 (z0),
		 z0_res = svreinterpret_u16 (z0))

/*
** reinterpret_u16_u32_untied:
**	mov	z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (reinterpret_u16_u32_untied, svuint16_t, svuint32_t,
	     z0 = svreinterpret_u16_u32 (z4),
	     z0 = svreinterpret_u16 (z4))

/*
** reinterpret_u16_u64_tied1:
**	ret
*/
TEST_DUAL_Z_REV (reinterpret_u16_u64_tied1, svuint16_t, svuint64_t,
		 z0_res = svreinterpret_u16_u64 (z0),
		 z0_res = svreinterpret_u16 (z0))

/*
** reinterpret_u16_u64_untied:
**	mov	z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (reinterpret_u16_u64_untied, svuint16_t, svuint64_t,
	     z0 = svreinterpret_u16_u64 (z4),
	     z0 = svreinterpret_u16 (z4))
