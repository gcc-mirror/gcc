/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshl_s32_m_tied1:
**	sqshl	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (qshl_s32_m_tied1, svint32_t, svint32_t,
	     z0 = svqshl_s32_m (p0, z0, z4),
	     z0 = svqshl_m (p0, z0, z4))

/*
** qshl_s32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sqshl	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (qshl_s32_m_tied2, svint32_t, svint32_t,
		 z0_res = svqshl_s32_m (p0, z4, z0),
		 z0_res = svqshl_m (p0, z4, z0))

/*
** qshl_s32_m_untied:
**	movprfx	z0, z1
**	sqshl	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (qshl_s32_m_untied, svint32_t, svint32_t,
	     z0 = svqshl_s32_m (p0, z1, z4),
	     z0 = svqshl_m (p0, z1, z4))

/*
** qshl_w0_s32_m_tied1:
**	mov	(z[0-9]+\.s), w0
**	sqshl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_s32_m_tied1, svint32_t, int32_t,
		 z0 = svqshl_n_s32_m (p0, z0, x0),
		 z0 = svqshl_m (p0, z0, x0))

/*
** qshl_w0_s32_m_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	sqshl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_s32_m_untied, svint32_t, int32_t,
		 z0 = svqshl_n_s32_m (p0, z1, x0),
		 z0 = svqshl_m (p0, z1, x0))

/*
** qshl_m32_s32_m:
**	asr	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (qshl_m32_s32_m, svint32_t,
		z0 = svqshl_n_s32_m (p0, z0, -32),
		z0 = svqshl_m (p0, z0, -32))

/*
** qshl_m2_s32_m:
**	asr	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_m2_s32_m, svint32_t,
		z0 = svqshl_n_s32_m (p0, z0, -2),
		z0 = svqshl_m (p0, z0, -2))

/*
** qshl_m1_s32_m_tied1:
**	asr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_s32_m_tied1, svint32_t,
		z0 = svqshl_n_s32_m (p0, z0, -1),
		z0 = svqshl_m (p0, z0, -1))

/*
** qshl_m1_s32_m_untied:
**	movprfx	z0, z1
**	asr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_s32_m_untied, svint32_t,
		z0 = svqshl_n_s32_m (p0, z1, -1),
		z0 = svqshl_m (p0, z1, -1))

/*
** qshl_1_s32_m_tied1:
**	sqshl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_s32_m_tied1, svint32_t,
		z0 = svqshl_n_s32_m (p0, z0, 1),
		z0 = svqshl_m (p0, z0, 1))

/*
** qshl_1_s32_m_untied:
**	movprfx	z0, z1
**	sqshl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_s32_m_untied, svint32_t,
		z0 = svqshl_n_s32_m (p0, z1, 1),
		z0 = svqshl_m (p0, z1, 1))

/*
** qshl_2_s32_m:
**	sqshl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_2_s32_m, svint32_t,
		z0 = svqshl_n_s32_m (p0, z0, 2),
		z0 = svqshl_m (p0, z0, 2))

/*
** qshl_31_s32_m:
**	sqshl	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (qshl_31_s32_m, svint32_t,
		z0 = svqshl_n_s32_m (p0, z0, 31),
		z0 = svqshl_m (p0, z0, 31))

/*
** qshl_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	sqshl	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (qshl_s32_z_tied1, svint32_t, svint32_t,
	     z0 = svqshl_s32_z (p0, z0, z4),
	     z0 = svqshl_z (p0, z0, z4))

/*
** qshl_s32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	sqshlr	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z_REV (qshl_s32_z_tied2, svint32_t, svint32_t,
		 z0_res = svqshl_s32_z (p0, z4, z0),
		 z0_res = svqshl_z (p0, z4, z0))

/*
** qshl_s32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	sqshl	z0\.s, p0/m, z0\.s, z4\.s
** |
**	movprfx	z0\.s, p0/z, z4\.s
**	sqshlr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_DUAL_Z (qshl_s32_z_untied, svint32_t, svint32_t,
	     z0 = svqshl_s32_z (p0, z1, z4),
	     z0 = svqshl_z (p0, z1, z4))

/*
** qshl_w0_s32_z_tied1:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	sqshl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_s32_z_tied1, svint32_t, int32_t,
		 z0 = svqshl_n_s32_z (p0, z0, x0),
		 z0 = svqshl_z (p0, z0, x0))

/*
** qshl_w0_s32_z_untied:
**	mov	(z[0-9]+\.s), w0
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	sqshl	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	sqshlr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_s32_z_untied, svint32_t, int32_t,
		 z0 = svqshl_n_s32_z (p0, z1, x0),
		 z0 = svqshl_z (p0, z1, x0))

/*
** qshl_m32_s32_z:
**	movprfx	z0\.s, p0/z, z0\.s
**	asr	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (qshl_m32_s32_z, svint32_t,
		z0 = svqshl_n_s32_z (p0, z0, -32),
		z0 = svqshl_z (p0, z0, -32))

/*
** qshl_m2_s32_z:
**	movprfx	z0\.s, p0/z, z0\.s
**	asr	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_m2_s32_z, svint32_t,
		z0 = svqshl_n_s32_z (p0, z0, -2),
		z0 = svqshl_z (p0, z0, -2))

/*
** qshl_m1_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	asr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_s32_z_tied1, svint32_t,
		z0 = svqshl_n_s32_z (p0, z0, -1),
		z0 = svqshl_z (p0, z0, -1))

/*
** qshl_m1_s32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	asr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_s32_z_untied, svint32_t,
		z0 = svqshl_n_s32_z (p0, z1, -1),
		z0 = svqshl_z (p0, z1, -1))

/*
** qshl_1_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	sqshl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_s32_z_tied1, svint32_t,
		z0 = svqshl_n_s32_z (p0, z0, 1),
		z0 = svqshl_z (p0, z0, 1))

/*
** qshl_1_s32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	sqshl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_s32_z_untied, svint32_t,
		z0 = svqshl_n_s32_z (p0, z1, 1),
		z0 = svqshl_z (p0, z1, 1))

/*
** qshl_2_s32_z:
**	movprfx	z0\.s, p0/z, z0\.s
**	sqshl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_2_s32_z, svint32_t,
		z0 = svqshl_n_s32_z (p0, z0, 2),
		z0 = svqshl_z (p0, z0, 2))

/*
** qshl_31_s32_z:
**	movprfx	z0\.s, p0/z, z0\.s
**	sqshl	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (qshl_31_s32_z, svint32_t,
		z0 = svqshl_n_s32_z (p0, z0, 31),
		z0 = svqshl_z (p0, z0, 31))

/*
** qshl_s32_x_tied1:
**	sqshl	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (qshl_s32_x_tied1, svint32_t, svint32_t,
	     z0 = svqshl_s32_x (p0, z0, z4),
	     z0 = svqshl_x (p0, z0, z4))

/*
** qshl_s32_x_tied2:
**	sqshlr	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z_REV (qshl_s32_x_tied2, svint32_t, svint32_t,
		 z0_res = svqshl_s32_x (p0, z4, z0),
		 z0_res = svqshl_x (p0, z4, z0))

/*
** qshl_s32_x_untied:
** (
**	movprfx	z0, z1
**	sqshl	z0\.s, p0/m, z0\.s, z4\.s
** |
**	movprfx	z0, z4
**	sqshlr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_DUAL_Z (qshl_s32_x_untied, svint32_t, svint32_t,
	     z0 = svqshl_s32_x (p0, z1, z4),
	     z0 = svqshl_x (p0, z1, z4))

/*
** qshl_w0_s32_x_tied1:
**	mov	(z[0-9]+\.s), w0
**	sqshl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_s32_x_tied1, svint32_t, int32_t,
		 z0 = svqshl_n_s32_x (p0, z0, x0),
		 z0 = svqshl_x (p0, z0, x0))

/*
** qshl_w0_s32_x_untied:
**	mov	z0\.s, w0
**	sqshlr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_s32_x_untied, svint32_t, int32_t,
		 z0 = svqshl_n_s32_x (p0, z1, x0),
		 z0 = svqshl_x (p0, z1, x0))

/*
** qshl_m32_s32_x:
**	asr	z0\.s, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (qshl_m32_s32_x, svint32_t,
		z0 = svqshl_n_s32_x (p0, z0, -32),
		z0 = svqshl_x (p0, z0, -32))

/*
** qshl_m2_s32_x:
**	asr	z0\.s, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_m2_s32_x, svint32_t,
		z0 = svqshl_n_s32_x (p0, z0, -2),
		z0 = svqshl_x (p0, z0, -2))

/*
** qshl_m1_s32_x_tied1:
**	asr	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_s32_x_tied1, svint32_t,
		z0 = svqshl_n_s32_x (p0, z0, -1),
		z0 = svqshl_x (p0, z0, -1))

/*
** qshl_m1_s32_x_untied:
**	asr	z0\.s, z1\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_s32_x_untied, svint32_t,
		z0 = svqshl_n_s32_x (p0, z1, -1),
		z0 = svqshl_x (p0, z1, -1))

/*
** qshl_1_s32_x_tied1:
**	sqshl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_s32_x_tied1, svint32_t,
		z0 = svqshl_n_s32_x (p0, z0, 1),
		z0 = svqshl_x (p0, z0, 1))

/*
** qshl_1_s32_x_untied:
**	movprfx	z0, z1
**	sqshl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_s32_x_untied, svint32_t,
		z0 = svqshl_n_s32_x (p0, z1, 1),
		z0 = svqshl_x (p0, z1, 1))

/*
** qshl_2_s32_x:
**	sqshl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_2_s32_x, svint32_t,
		z0 = svqshl_n_s32_x (p0, z0, 2),
		z0 = svqshl_x (p0, z0, 2))

/*
** qshl_31_s32_x:
**	sqshl	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (qshl_31_s32_x, svint32_t,
		z0 = svqshl_n_s32_x (p0, z0, 31),
		z0 = svqshl_x (p0, z0, 31))
