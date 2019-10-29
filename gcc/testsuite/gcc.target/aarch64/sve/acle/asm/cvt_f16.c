/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cvt_f16_f32_m_tied1:
**	fcvt	z0\.h, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (cvt_f16_f32_m_tied1, svfloat16_t, svfloat32_t,
	     z0 = svcvt_f16_f32_m (z0, p0, z4),
	     z0 = svcvt_f16_m (z0, p0, z4))

/*
** cvt_f16_f32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fcvt	z0\.h, p0/m, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (cvt_f16_f32_m_tied2, svfloat16_t, svfloat32_t,
		 z0_res = svcvt_f16_f32_m (z4, p0, z0),
		 z0_res = svcvt_f16_m (z4, p0, z0))

/*
** cvt_f16_f32_m_untied:
**	movprfx	z0, z1
**	fcvt	z0\.h, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (cvt_f16_f32_m_untied, svfloat16_t, svfloat32_t,
	     z0 = svcvt_f16_f32_m (z1, p0, z4),
	     z0 = svcvt_f16_m (z1, p0, z4))

/*
** cvt_f16_f64_m_tied1:
**	fcvt	z0\.h, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cvt_f16_f64_m_tied1, svfloat16_t, svfloat64_t,
	     z0 = svcvt_f16_f64_m (z0, p0, z4),
	     z0 = svcvt_f16_m (z0, p0, z4))

/*
** cvt_f16_f64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z4
**	fcvt	z0\.h, p0/m, \1
**	ret
*/
TEST_DUAL_Z_REV (cvt_f16_f64_m_tied2, svfloat16_t, svfloat64_t,
		 z0_res = svcvt_f16_f64_m (z4, p0, z0),
		 z0_res = svcvt_f16_m (z4, p0, z0))

/*
** cvt_f16_f64_m_untied:
**	movprfx	z0, z1
**	fcvt	z0\.h, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cvt_f16_f64_m_untied, svfloat16_t, svfloat64_t,
	     z0 = svcvt_f16_f64_m (z1, p0, z4),
	     z0 = svcvt_f16_m (z1, p0, z4))

/*
** cvt_f16_s16_m_tied1:
**	scvtf	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (cvt_f16_s16_m_tied1, svfloat16_t, svint16_t,
	     z0 = svcvt_f16_s16_m (z0, p0, z4),
	     z0 = svcvt_f16_m (z0, p0, z4))

/*
** cvt_f16_s16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	scvtf	z0\.h, p0/m, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (cvt_f16_s16_m_tied2, svfloat16_t, svint16_t,
		 z0_res = svcvt_f16_s16_m (z4, p0, z0),
		 z0_res = svcvt_f16_m (z4, p0, z0))

/*
** cvt_f16_s16_m_untied:
**	movprfx	z0, z1
**	scvtf	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (cvt_f16_s16_m_untied, svfloat16_t, svint16_t,
	     z0 = svcvt_f16_s16_m (z1, p0, z4),
	     z0 = svcvt_f16_m (z1, p0, z4))

/*
** cvt_f16_s32_m_tied1:
**	scvtf	z0\.h, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (cvt_f16_s32_m_tied1, svfloat16_t, svint32_t,
	     z0 = svcvt_f16_s32_m (z0, p0, z4),
	     z0 = svcvt_f16_m (z0, p0, z4))

/*
** cvt_f16_s32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	scvtf	z0\.h, p0/m, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (cvt_f16_s32_m_tied2, svfloat16_t, svint32_t,
		 z0_res = svcvt_f16_s32_m (z4, p0, z0),
		 z0_res = svcvt_f16_m (z4, p0, z0))

/*
** cvt_f16_s32_m_untied:
**	movprfx	z0, z1
**	scvtf	z0\.h, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (cvt_f16_s32_m_untied, svfloat16_t, svint32_t,
	     z0 = svcvt_f16_s32_m (z1, p0, z4),
	     z0 = svcvt_f16_m (z1, p0, z4))

/*
** cvt_f16_s64_m_tied1:
**	scvtf	z0\.h, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cvt_f16_s64_m_tied1, svfloat16_t, svint64_t,
	     z0 = svcvt_f16_s64_m (z0, p0, z4),
	     z0 = svcvt_f16_m (z0, p0, z4))

/*
** cvt_f16_s64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z4
**	scvtf	z0\.h, p0/m, \1
**	ret
*/
TEST_DUAL_Z_REV (cvt_f16_s64_m_tied2, svfloat16_t, svint64_t,
		 z0_res = svcvt_f16_s64_m (z4, p0, z0),
		 z0_res = svcvt_f16_m (z4, p0, z0))

/*
** cvt_f16_s64_m_untied:
**	movprfx	z0, z1
**	scvtf	z0\.h, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cvt_f16_s64_m_untied, svfloat16_t, svint64_t,
	     z0 = svcvt_f16_s64_m (z1, p0, z4),
	     z0 = svcvt_f16_m (z1, p0, z4))

/*
** cvt_f16_u16_m_tied1:
**	ucvtf	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (cvt_f16_u16_m_tied1, svfloat16_t, svuint16_t,
	     z0 = svcvt_f16_u16_m (z0, p0, z4),
	     z0 = svcvt_f16_m (z0, p0, z4))

/*
** cvt_f16_u16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	ucvtf	z0\.h, p0/m, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (cvt_f16_u16_m_tied2, svfloat16_t, svuint16_t,
		 z0_res = svcvt_f16_u16_m (z4, p0, z0),
		 z0_res = svcvt_f16_m (z4, p0, z0))

/*
** cvt_f16_u16_m_untied:
**	movprfx	z0, z1
**	ucvtf	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (cvt_f16_u16_m_untied, svfloat16_t, svuint16_t,
	     z0 = svcvt_f16_u16_m (z1, p0, z4),
	     z0 = svcvt_f16_m (z1, p0, z4))

/*
** cvt_f16_u32_m_tied1:
**	ucvtf	z0\.h, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (cvt_f16_u32_m_tied1, svfloat16_t, svuint32_t,
	     z0 = svcvt_f16_u32_m (z0, p0, z4),
	     z0 = svcvt_f16_m (z0, p0, z4))

/*
** cvt_f16_u32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	ucvtf	z0\.h, p0/m, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (cvt_f16_u32_m_tied2, svfloat16_t, svuint32_t,
		 z0_res = svcvt_f16_u32_m (z4, p0, z0),
		 z0_res = svcvt_f16_m (z4, p0, z0))

/*
** cvt_f16_u32_m_untied:
**	movprfx	z0, z1
**	ucvtf	z0\.h, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (cvt_f16_u32_m_untied, svfloat16_t, svuint32_t,
	     z0 = svcvt_f16_u32_m (z1, p0, z4),
	     z0 = svcvt_f16_m (z1, p0, z4))

/*
** cvt_f16_u64_m_tied1:
**	ucvtf	z0\.h, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cvt_f16_u64_m_tied1, svfloat16_t, svuint64_t,
	     z0 = svcvt_f16_u64_m (z0, p0, z4),
	     z0 = svcvt_f16_m (z0, p0, z4))

/*
** cvt_f16_u64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z4
**	ucvtf	z0\.h, p0/m, \1
**	ret
*/
TEST_DUAL_Z_REV (cvt_f16_u64_m_tied2, svfloat16_t, svuint64_t,
		 z0_res = svcvt_f16_u64_m (z4, p0, z0),
		 z0_res = svcvt_f16_m (z4, p0, z0))

/*
** cvt_f16_u64_m_untied:
**	movprfx	z0, z1
**	ucvtf	z0\.h, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cvt_f16_u64_m_untied, svfloat16_t, svuint64_t,
	     z0 = svcvt_f16_u64_m (z1, p0, z4),
	     z0 = svcvt_f16_m (z1, p0, z4))

/*
** cvt_f16_f32_z_tied1:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.s, p0/z, \1\.s
**	fcvt	z0\.h, p0/m, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (cvt_f16_f32_z_tied1, svfloat16_t, svfloat32_t,
		 z0_res = svcvt_f16_f32_z (p0, z0),
		 z0_res = svcvt_f16_z (p0, z0))

/*
** cvt_f16_f32_z_untied:
**	movprfx	z0\.s, p0/z, z4\.s
**	fcvt	z0\.h, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (cvt_f16_f32_z_untied, svfloat16_t, svfloat32_t,
	     z0 = svcvt_f16_f32_z (p0, z4),
	     z0 = svcvt_f16_z (p0, z4))

/*
** cvt_f16_f64_z_tied1:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.d, p0/z, \1
**	fcvt	z0\.h, p0/m, \1
**	ret
*/
TEST_DUAL_Z_REV (cvt_f16_f64_z_tied1, svfloat16_t, svfloat64_t,
		 z0_res = svcvt_f16_f64_z (p0, z0),
		 z0_res = svcvt_f16_z (p0, z0))

/*
** cvt_f16_f64_z_untied:
**	movprfx	z0\.d, p0/z, z4\.d
**	fcvt	z0\.h, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cvt_f16_f64_z_untied, svfloat16_t, svfloat64_t,
	     z0 = svcvt_f16_f64_z (p0, z4),
	     z0 = svcvt_f16_z (p0, z4))

/*
** cvt_f16_s16_z_tied1:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.h, p0/z, \1\.h
**	scvtf	z0\.h, p0/m, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (cvt_f16_s16_z_tied1, svfloat16_t, svint16_t,
		 z0_res = svcvt_f16_s16_z (p0, z0),
		 z0_res = svcvt_f16_z (p0, z0))

/*
** cvt_f16_s16_z_untied:
**	movprfx	z0\.h, p0/z, z4\.h
**	scvtf	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (cvt_f16_s16_z_untied, svfloat16_t, svint16_t,
	     z0 = svcvt_f16_s16_z (p0, z4),
	     z0 = svcvt_f16_z (p0, z4))

/*
** cvt_f16_s32_z_tied1:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.s, p0/z, \1\.s
**	scvtf	z0\.h, p0/m, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (cvt_f16_s32_z_tied1, svfloat16_t, svint32_t,
		 z0_res = svcvt_f16_s32_z (p0, z0),
		 z0_res = svcvt_f16_z (p0, z0))

/*
** cvt_f16_s32_z_untied:
**	movprfx	z0\.s, p0/z, z4\.s
**	scvtf	z0\.h, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (cvt_f16_s32_z_untied, svfloat16_t, svint32_t,
	     z0 = svcvt_f16_s32_z (p0, z4),
	     z0 = svcvt_f16_z (p0, z4))

/*
** cvt_f16_s64_z_tied1:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.d, p0/z, \1
**	scvtf	z0\.h, p0/m, \1
**	ret
*/
TEST_DUAL_Z_REV (cvt_f16_s64_z_tied1, svfloat16_t, svint64_t,
		 z0_res = svcvt_f16_s64_z (p0, z0),
		 z0_res = svcvt_f16_z (p0, z0))

/*
** cvt_f16_s64_z_untied:
**	movprfx	z0\.d, p0/z, z4\.d
**	scvtf	z0\.h, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cvt_f16_s64_z_untied, svfloat16_t, svint64_t,
	     z0 = svcvt_f16_s64_z (p0, z4),
	     z0 = svcvt_f16_z (p0, z4))

/*
** cvt_f16_u16_z_tied1:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.h, p0/z, \1\.h
**	ucvtf	z0\.h, p0/m, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (cvt_f16_u16_z_tied1, svfloat16_t, svuint16_t,
		 z0_res = svcvt_f16_u16_z (p0, z0),
		 z0_res = svcvt_f16_z (p0, z0))

/*
** cvt_f16_u16_z_untied:
**	movprfx	z0\.h, p0/z, z4\.h
**	ucvtf	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (cvt_f16_u16_z_untied, svfloat16_t, svuint16_t,
	     z0 = svcvt_f16_u16_z (p0, z4),
	     z0 = svcvt_f16_z (p0, z4))

/*
** cvt_f16_u32_z_tied1:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.s, p0/z, \1\.s
**	ucvtf	z0\.h, p0/m, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (cvt_f16_u32_z_tied1, svfloat16_t, svuint32_t,
		 z0_res = svcvt_f16_u32_z (p0, z0),
		 z0_res = svcvt_f16_z (p0, z0))

/*
** cvt_f16_u32_z_untied:
**	movprfx	z0\.s, p0/z, z4\.s
**	ucvtf	z0\.h, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (cvt_f16_u32_z_untied, svfloat16_t, svuint32_t,
	     z0 = svcvt_f16_u32_z (p0, z4),
	     z0 = svcvt_f16_z (p0, z4))

/*
** cvt_f16_u64_z_tied1:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.d, p0/z, \1
**	ucvtf	z0\.h, p0/m, \1
**	ret
*/
TEST_DUAL_Z_REV (cvt_f16_u64_z_tied1, svfloat16_t, svuint64_t,
		 z0_res = svcvt_f16_u64_z (p0, z0),
		 z0_res = svcvt_f16_z (p0, z0))

/*
** cvt_f16_u64_z_untied:
**	movprfx	z0\.d, p0/z, z4\.d
**	ucvtf	z0\.h, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cvt_f16_u64_z_untied, svfloat16_t, svuint64_t,
	     z0 = svcvt_f16_u64_z (p0, z4),
	     z0 = svcvt_f16_z (p0, z4))

/*
** cvt_f16_f32_x_tied1:
**	fcvt	z0\.h, p0/m, z0\.s
**	ret
*/
TEST_DUAL_Z_REV (cvt_f16_f32_x_tied1, svfloat16_t, svfloat32_t,
		 z0_res = svcvt_f16_f32_x (p0, z0),
		 z0_res = svcvt_f16_x (p0, z0))

/*
** cvt_f16_f32_x_untied:
**	fcvt	z0\.h, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (cvt_f16_f32_x_untied, svfloat16_t, svfloat32_t,
	     z0 = svcvt_f16_f32_x (p0, z4),
	     z0 = svcvt_f16_x (p0, z4))

/*
** cvt_f16_f64_x_tied1:
**	fcvt	z0\.h, p0/m, z0\.d
**	ret
*/
TEST_DUAL_Z_REV (cvt_f16_f64_x_tied1, svfloat16_t, svfloat64_t,
		 z0_res = svcvt_f16_f64_x (p0, z0),
		 z0_res = svcvt_f16_x (p0, z0))

/*
** cvt_f16_f64_x_untied:
**	fcvt	z0\.h, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cvt_f16_f64_x_untied, svfloat16_t, svfloat64_t,
	     z0 = svcvt_f16_f64_x (p0, z4),
	     z0 = svcvt_f16_x (p0, z4))

/*
** cvt_f16_s16_x_tied1:
**	scvtf	z0\.h, p0/m, z0\.h
**	ret
*/
TEST_DUAL_Z_REV (cvt_f16_s16_x_tied1, svfloat16_t, svint16_t,
		 z0_res = svcvt_f16_s16_x (p0, z0),
		 z0_res = svcvt_f16_x (p0, z0))

/*
** cvt_f16_s16_x_untied:
**	scvtf	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (cvt_f16_s16_x_untied, svfloat16_t, svint16_t,
	     z0 = svcvt_f16_s16_x (p0, z4),
	     z0 = svcvt_f16_x (p0, z4))

/*
** cvt_f16_s32_x_tied1:
**	scvtf	z0\.h, p0/m, z0\.s
**	ret
*/
TEST_DUAL_Z_REV (cvt_f16_s32_x_tied1, svfloat16_t, svint32_t,
		 z0_res = svcvt_f16_s32_x (p0, z0),
		 z0_res = svcvt_f16_x (p0, z0))

/*
** cvt_f16_s32_x_untied:
**	scvtf	z0\.h, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (cvt_f16_s32_x_untied, svfloat16_t, svint32_t,
	     z0 = svcvt_f16_s32_x (p0, z4),
	     z0 = svcvt_f16_x (p0, z4))

/*
** cvt_f16_s64_x_tied1:
**	scvtf	z0\.h, p0/m, z0\.d
**	ret
*/
TEST_DUAL_Z_REV (cvt_f16_s64_x_tied1, svfloat16_t, svint64_t,
		 z0_res = svcvt_f16_s64_x (p0, z0),
		 z0_res = svcvt_f16_x (p0, z0))

/*
** cvt_f16_s64_x_untied:
**	scvtf	z0\.h, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cvt_f16_s64_x_untied, svfloat16_t, svint64_t,
	     z0 = svcvt_f16_s64_x (p0, z4),
	     z0 = svcvt_f16_x (p0, z4))

/*
** cvt_f16_u16_x_tied1:
**	ucvtf	z0\.h, p0/m, z0\.h
**	ret
*/
TEST_DUAL_Z_REV (cvt_f16_u16_x_tied1, svfloat16_t, svuint16_t,
		 z0_res = svcvt_f16_u16_x (p0, z0),
		 z0_res = svcvt_f16_x (p0, z0))

/*
** cvt_f16_u16_x_untied:
**	ucvtf	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (cvt_f16_u16_x_untied, svfloat16_t, svuint16_t,
	     z0 = svcvt_f16_u16_x (p0, z4),
	     z0 = svcvt_f16_x (p0, z4))

/*
** cvt_f16_u32_x_tied1:
**	ucvtf	z0\.h, p0/m, z0\.s
**	ret
*/
TEST_DUAL_Z_REV (cvt_f16_u32_x_tied1, svfloat16_t, svuint32_t,
		 z0_res = svcvt_f16_u32_x (p0, z0),
		 z0_res = svcvt_f16_x (p0, z0))

/*
** cvt_f16_u32_x_untied:
**	ucvtf	z0\.h, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (cvt_f16_u32_x_untied, svfloat16_t, svuint32_t,
	     z0 = svcvt_f16_u32_x (p0, z4),
	     z0 = svcvt_f16_x (p0, z4))

/*
** cvt_f16_u64_x_tied1:
**	ucvtf	z0\.h, p0/m, z0\.d
**	ret
*/
TEST_DUAL_Z_REV (cvt_f16_u64_x_tied1, svfloat16_t, svuint64_t,
		 z0_res = svcvt_f16_u64_x (p0, z0),
		 z0_res = svcvt_f16_x (p0, z0))

/*
** cvt_f16_u64_x_untied:
**	ucvtf	z0\.h, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cvt_f16_u64_x_untied, svfloat16_t, svuint64_t,
	     z0 = svcvt_f16_u64_x (p0, z4),
	     z0 = svcvt_f16_x (p0, z4))

/*
** ptrue_cvt_f16_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z_REV (ptrue_cvt_f16_f32_x_tied1, svfloat16_t, svfloat32_t,
		 z0_res = svcvt_f16_f32_x (svptrue_b32 (), z0),
		 z0_res = svcvt_f16_x (svptrue_b32 (), z0))

/*
** ptrue_cvt_f16_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z (ptrue_cvt_f16_f32_x_untied, svfloat16_t, svfloat32_t,
	     z0 = svcvt_f16_f32_x (svptrue_b32 (), z4),
	     z0 = svcvt_f16_x (svptrue_b32 (), z4))

/*
** ptrue_cvt_f16_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z_REV (ptrue_cvt_f16_f64_x_tied1, svfloat16_t, svfloat64_t,
		 z0_res = svcvt_f16_f64_x (svptrue_b64 (), z0),
		 z0_res = svcvt_f16_x (svptrue_b64 (), z0))

/*
** ptrue_cvt_f16_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z (ptrue_cvt_f16_f64_x_untied, svfloat16_t, svfloat64_t,
	     z0 = svcvt_f16_f64_x (svptrue_b64 (), z4),
	     z0 = svcvt_f16_x (svptrue_b64 (), z4))

/*
** ptrue_cvt_f16_s16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z_REV (ptrue_cvt_f16_s16_x_tied1, svfloat16_t, svint16_t,
		 z0_res = svcvt_f16_s16_x (svptrue_b16 (), z0),
		 z0_res = svcvt_f16_x (svptrue_b16 (), z0))

/*
** ptrue_cvt_f16_s16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z (ptrue_cvt_f16_s16_x_untied, svfloat16_t, svint16_t,
	     z0 = svcvt_f16_s16_x (svptrue_b16 (), z4),
	     z0 = svcvt_f16_x (svptrue_b16 (), z4))

/*
** ptrue_cvt_f16_s32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z_REV (ptrue_cvt_f16_s32_x_tied1, svfloat16_t, svint32_t,
		 z0_res = svcvt_f16_s32_x (svptrue_b32 (), z0),
		 z0_res = svcvt_f16_x (svptrue_b32 (), z0))

/*
** ptrue_cvt_f16_s32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z (ptrue_cvt_f16_s32_x_untied, svfloat16_t, svint32_t,
	     z0 = svcvt_f16_s32_x (svptrue_b32 (), z4),
	     z0 = svcvt_f16_x (svptrue_b32 (), z4))

/*
** ptrue_cvt_f16_s64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z_REV (ptrue_cvt_f16_s64_x_tied1, svfloat16_t, svint64_t,
		 z0_res = svcvt_f16_s64_x (svptrue_b64 (), z0),
		 z0_res = svcvt_f16_x (svptrue_b64 (), z0))

/*
** ptrue_cvt_f16_s64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z (ptrue_cvt_f16_s64_x_untied, svfloat16_t, svint64_t,
	     z0 = svcvt_f16_s64_x (svptrue_b64 (), z4),
	     z0 = svcvt_f16_x (svptrue_b64 (), z4))

/*
** ptrue_cvt_f16_u16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z_REV (ptrue_cvt_f16_u16_x_tied1, svfloat16_t, svuint16_t,
		 z0_res = svcvt_f16_u16_x (svptrue_b16 (), z0),
		 z0_res = svcvt_f16_x (svptrue_b16 (), z0))

/*
** ptrue_cvt_f16_u16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z (ptrue_cvt_f16_u16_x_untied, svfloat16_t, svuint16_t,
	     z0 = svcvt_f16_u16_x (svptrue_b16 (), z4),
	     z0 = svcvt_f16_x (svptrue_b16 (), z4))

/*
** ptrue_cvt_f16_u32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z_REV (ptrue_cvt_f16_u32_x_tied1, svfloat16_t, svuint32_t,
		 z0_res = svcvt_f16_u32_x (svptrue_b32 (), z0),
		 z0_res = svcvt_f16_x (svptrue_b32 (), z0))

/*
** ptrue_cvt_f16_u32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z (ptrue_cvt_f16_u32_x_untied, svfloat16_t, svuint32_t,
	     z0 = svcvt_f16_u32_x (svptrue_b32 (), z4),
	     z0 = svcvt_f16_x (svptrue_b32 (), z4))

/*
** ptrue_cvt_f16_u64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z_REV (ptrue_cvt_f16_u64_x_tied1, svfloat16_t, svuint64_t,
		 z0_res = svcvt_f16_u64_x (svptrue_b64 (), z0),
		 z0_res = svcvt_f16_x (svptrue_b64 (), z0))

/*
** ptrue_cvt_f16_u64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z (ptrue_cvt_f16_u64_x_untied, svfloat16_t, svuint64_t,
	     z0 = svcvt_f16_u64_x (svptrue_b64 (), z4),
	     z0 = svcvt_f16_x (svptrue_b64 (), z4))
