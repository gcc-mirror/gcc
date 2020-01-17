/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshl_s16_m_tied1:
**	sqshl	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (qshl_s16_m_tied1, svint16_t, svint16_t,
	     z0 = svqshl_s16_m (p0, z0, z4),
	     z0 = svqshl_m (p0, z0, z4))

/*
** qshl_s16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sqshl	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (qshl_s16_m_tied2, svint16_t, svint16_t,
		 z0_res = svqshl_s16_m (p0, z4, z0),
		 z0_res = svqshl_m (p0, z4, z0))

/*
** qshl_s16_m_untied:
**	movprfx	z0, z1
**	sqshl	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (qshl_s16_m_untied, svint16_t, svint16_t,
	     z0 = svqshl_s16_m (p0, z1, z4),
	     z0 = svqshl_m (p0, z1, z4))

/*
** qshl_w0_s16_m_tied1:
**	mov	(z[0-9]+\.h), w0
**	sqshl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_s16_m_tied1, svint16_t, int16_t,
		 z0 = svqshl_n_s16_m (p0, z0, x0),
		 z0 = svqshl_m (p0, z0, x0))

/*
** qshl_w0_s16_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	sqshl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_s16_m_untied, svint16_t, int16_t,
		 z0 = svqshl_n_s16_m (p0, z1, x0),
		 z0 = svqshl_m (p0, z1, x0))

/*
** qshl_m16_s16_m:
**	asr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (qshl_m16_s16_m, svint16_t,
		z0 = svqshl_n_s16_m (p0, z0, -16),
		z0 = svqshl_m (p0, z0, -16))

/*
** qshl_m2_s16_m:
**	asr	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_m2_s16_m, svint16_t,
		z0 = svqshl_n_s16_m (p0, z0, -2),
		z0 = svqshl_m (p0, z0, -2))

/*
** qshl_m1_s16_m_tied1:
**	asr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_s16_m_tied1, svint16_t,
		z0 = svqshl_n_s16_m (p0, z0, -1),
		z0 = svqshl_m (p0, z0, -1))

/*
** qshl_m1_s16_m_untied:
**	movprfx	z0, z1
**	asr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_s16_m_untied, svint16_t,
		z0 = svqshl_n_s16_m (p0, z1, -1),
		z0 = svqshl_m (p0, z1, -1))

/*
** qshl_1_s16_m_tied1:
**	sqshl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_s16_m_tied1, svint16_t,
		z0 = svqshl_n_s16_m (p0, z0, 1),
		z0 = svqshl_m (p0, z0, 1))

/*
** qshl_1_s16_m_untied:
**	movprfx	z0, z1
**	sqshl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_s16_m_untied, svint16_t,
		z0 = svqshl_n_s16_m (p0, z1, 1),
		z0 = svqshl_m (p0, z1, 1))

/*
** qshl_2_s16_m:
**	sqshl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_2_s16_m, svint16_t,
		z0 = svqshl_n_s16_m (p0, z0, 2),
		z0 = svqshl_m (p0, z0, 2))

/*
** qshl_15_s16_m:
**	sqshl	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (qshl_15_s16_m, svint16_t,
		z0 = svqshl_n_s16_m (p0, z0, 15),
		z0 = svqshl_m (p0, z0, 15))

/*
** qshl_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	sqshl	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (qshl_s16_z_tied1, svint16_t, svint16_t,
	     z0 = svqshl_s16_z (p0, z0, z4),
	     z0 = svqshl_z (p0, z0, z4))

/*
** qshl_s16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	sqshlr	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z_REV (qshl_s16_z_tied2, svint16_t, svint16_t,
		 z0_res = svqshl_s16_z (p0, z4, z0),
		 z0_res = svqshl_z (p0, z4, z0))

/*
** qshl_s16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	sqshl	z0\.h, p0/m, z0\.h, z4\.h
** |
**	movprfx	z0\.h, p0/z, z4\.h
**	sqshlr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_DUAL_Z (qshl_s16_z_untied, svint16_t, svint16_t,
	     z0 = svqshl_s16_z (p0, z1, z4),
	     z0 = svqshl_z (p0, z1, z4))

/*
** qshl_w0_s16_z_tied1:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z0\.h
**	sqshl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_s16_z_tied1, svint16_t, int16_t,
		 z0 = svqshl_n_s16_z (p0, z0, x0),
		 z0 = svqshl_z (p0, z0, x0))

/*
** qshl_w0_s16_z_untied:
**	mov	(z[0-9]+\.h), w0
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	sqshl	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	sqshlr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_s16_z_untied, svint16_t, int16_t,
		 z0 = svqshl_n_s16_z (p0, z1, x0),
		 z0 = svqshl_z (p0, z1, x0))

/*
** qshl_m16_s16_z:
**	movprfx	z0\.h, p0/z, z0\.h
**	asr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (qshl_m16_s16_z, svint16_t,
		z0 = svqshl_n_s16_z (p0, z0, -16),
		z0 = svqshl_z (p0, z0, -16))

/*
** qshl_m2_s16_z:
**	movprfx	z0\.h, p0/z, z0\.h
**	asr	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_m2_s16_z, svint16_t,
		z0 = svqshl_n_s16_z (p0, z0, -2),
		z0 = svqshl_z (p0, z0, -2))

/*
** qshl_m1_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	asr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_s16_z_tied1, svint16_t,
		z0 = svqshl_n_s16_z (p0, z0, -1),
		z0 = svqshl_z (p0, z0, -1))

/*
** qshl_m1_s16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	asr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_s16_z_untied, svint16_t,
		z0 = svqshl_n_s16_z (p0, z1, -1),
		z0 = svqshl_z (p0, z1, -1))

/*
** qshl_1_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	sqshl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_s16_z_tied1, svint16_t,
		z0 = svqshl_n_s16_z (p0, z0, 1),
		z0 = svqshl_z (p0, z0, 1))

/*
** qshl_1_s16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	sqshl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_s16_z_untied, svint16_t,
		z0 = svqshl_n_s16_z (p0, z1, 1),
		z0 = svqshl_z (p0, z1, 1))

/*
** qshl_2_s16_z:
**	movprfx	z0\.h, p0/z, z0\.h
**	sqshl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_2_s16_z, svint16_t,
		z0 = svqshl_n_s16_z (p0, z0, 2),
		z0 = svqshl_z (p0, z0, 2))

/*
** qshl_15_s16_z:
**	movprfx	z0\.h, p0/z, z0\.h
**	sqshl	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (qshl_15_s16_z, svint16_t,
		z0 = svqshl_n_s16_z (p0, z0, 15),
		z0 = svqshl_z (p0, z0, 15))

/*
** qshl_s16_x_tied1:
**	sqshl	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (qshl_s16_x_tied1, svint16_t, svint16_t,
	     z0 = svqshl_s16_x (p0, z0, z4),
	     z0 = svqshl_x (p0, z0, z4))

/*
** qshl_s16_x_tied2:
**	sqshlr	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z_REV (qshl_s16_x_tied2, svint16_t, svint16_t,
		 z0_res = svqshl_s16_x (p0, z4, z0),
		 z0_res = svqshl_x (p0, z4, z0))

/*
** qshl_s16_x_untied:
** (
**	movprfx	z0, z1
**	sqshl	z0\.h, p0/m, z0\.h, z4\.h
** |
**	movprfx	z0, z4
**	sqshlr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_DUAL_Z (qshl_s16_x_untied, svint16_t, svint16_t,
	     z0 = svqshl_s16_x (p0, z1, z4),
	     z0 = svqshl_x (p0, z1, z4))

/*
** qshl_w0_s16_x_tied1:
**	mov	(z[0-9]+\.h), w0
**	sqshl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_s16_x_tied1, svint16_t, int16_t,
		 z0 = svqshl_n_s16_x (p0, z0, x0),
		 z0 = svqshl_x (p0, z0, x0))

/*
** qshl_w0_s16_x_untied:
**	mov	z0\.h, w0
**	sqshlr	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_s16_x_untied, svint16_t, int16_t,
		 z0 = svqshl_n_s16_x (p0, z1, x0),
		 z0 = svqshl_x (p0, z1, x0))

/*
** qshl_m16_s16_x:
**	asr	z0\.h, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (qshl_m16_s16_x, svint16_t,
		z0 = svqshl_n_s16_x (p0, z0, -16),
		z0 = svqshl_x (p0, z0, -16))

/*
** qshl_m2_s16_x:
**	asr	z0\.h, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_m2_s16_x, svint16_t,
		z0 = svqshl_n_s16_x (p0, z0, -2),
		z0 = svqshl_x (p0, z0, -2))

/*
** qshl_m1_s16_x_tied1:
**	asr	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_s16_x_tied1, svint16_t,
		z0 = svqshl_n_s16_x (p0, z0, -1),
		z0 = svqshl_x (p0, z0, -1))

/*
** qshl_m1_s16_x_untied:
**	asr	z0\.h, z1\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_s16_x_untied, svint16_t,
		z0 = svqshl_n_s16_x (p0, z1, -1),
		z0 = svqshl_x (p0, z1, -1))

/*
** qshl_1_s16_x_tied1:
**	sqshl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_s16_x_tied1, svint16_t,
		z0 = svqshl_n_s16_x (p0, z0, 1),
		z0 = svqshl_x (p0, z0, 1))

/*
** qshl_1_s16_x_untied:
**	movprfx	z0, z1
**	sqshl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_s16_x_untied, svint16_t,
		z0 = svqshl_n_s16_x (p0, z1, 1),
		z0 = svqshl_x (p0, z1, 1))

/*
** qshl_2_s16_x:
**	sqshl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_2_s16_x, svint16_t,
		z0 = svqshl_n_s16_x (p0, z0, 2),
		z0 = svqshl_x (p0, z0, 2))

/*
** qshl_15_s16_x:
**	sqshl	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (qshl_15_s16_x, svint16_t,
		z0 = svqshl_n_s16_x (p0, z0, 15),
		z0 = svqshl_x (p0, z0, 15))
