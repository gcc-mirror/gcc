/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrshl_s16_m_tied1:
**	sqrshl	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (qrshl_s16_m_tied1, svint16_t, svint16_t,
	     z0 = svqrshl_s16_m (p0, z0, z4),
	     z0 = svqrshl_m (p0, z0, z4))

/*
** qrshl_s16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sqrshl	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (qrshl_s16_m_tied2, svint16_t, svint16_t,
		 z0_res = svqrshl_s16_m (p0, z4, z0),
		 z0_res = svqrshl_m (p0, z4, z0))

/*
** qrshl_s16_m_untied:
**	movprfx	z0, z1
**	sqrshl	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (qrshl_s16_m_untied, svint16_t, svint16_t,
	     z0 = svqrshl_s16_m (p0, z1, z4),
	     z0 = svqrshl_m (p0, z1, z4))

/*
** qrshl_w0_s16_m_tied1:
**	mov	(z[0-9]+\.h), w0
**	sqrshl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_s16_m_tied1, svint16_t, int16_t,
		 z0 = svqrshl_n_s16_m (p0, z0, x0),
		 z0 = svqrshl_m (p0, z0, x0))

/*
** qrshl_w0_s16_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	sqrshl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_s16_m_untied, svint16_t, int16_t,
		 z0 = svqrshl_n_s16_m (p0, z1, x0),
		 z0 = svqrshl_m (p0, z1, x0))

/*
** qrshl_m16_s16_m:
**	srshr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (qrshl_m16_s16_m, svint16_t,
		z0 = svqrshl_n_s16_m (p0, z0, -16),
		z0 = svqrshl_m (p0, z0, -16))

/*
** qrshl_m2_s16_m:
**	srshr	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_m2_s16_m, svint16_t,
		z0 = svqrshl_n_s16_m (p0, z0, -2),
		z0 = svqrshl_m (p0, z0, -2))

/*
** qrshl_m1_s16_m_tied1:
**	srshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_s16_m_tied1, svint16_t,
		z0 = svqrshl_n_s16_m (p0, z0, -1),
		z0 = svqrshl_m (p0, z0, -1))

/*
** qrshl_m1_s16_m_untied:
**	movprfx	z0, z1
**	srshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_s16_m_untied, svint16_t,
		z0 = svqrshl_n_s16_m (p0, z1, -1),
		z0 = svqrshl_m (p0, z1, -1))

/*
** qrshl_1_s16_m_tied1:
**	sqshl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_s16_m_tied1, svint16_t,
		z0 = svqrshl_n_s16_m (p0, z0, 1),
		z0 = svqrshl_m (p0, z0, 1))

/*
** qrshl_1_s16_m_untied:
**	movprfx	z0, z1
**	sqshl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_s16_m_untied, svint16_t,
		z0 = svqrshl_n_s16_m (p0, z1, 1),
		z0 = svqrshl_m (p0, z1, 1))

/*
** qrshl_2_s16_m:
**	sqshl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_2_s16_m, svint16_t,
		z0 = svqrshl_n_s16_m (p0, z0, 2),
		z0 = svqrshl_m (p0, z0, 2))

/*
** qrshl_15_s16_m:
**	sqshl	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (qrshl_15_s16_m, svint16_t,
		z0 = svqrshl_n_s16_m (p0, z0, 15),
		z0 = svqrshl_m (p0, z0, 15))

/*
** qrshl_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	sqrshl	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (qrshl_s16_z_tied1, svint16_t, svint16_t,
	     z0 = svqrshl_s16_z (p0, z0, z4),
	     z0 = svqrshl_z (p0, z0, z4))

/*
** qrshl_s16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	sqrshlr	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z_REV (qrshl_s16_z_tied2, svint16_t, svint16_t,
		 z0_res = svqrshl_s16_z (p0, z4, z0),
		 z0_res = svqrshl_z (p0, z4, z0))

/*
** qrshl_s16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	sqrshl	z0\.h, p0/m, z0\.h, z4\.h
** |
**	movprfx	z0\.h, p0/z, z4\.h
**	sqrshlr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_DUAL_Z (qrshl_s16_z_untied, svint16_t, svint16_t,
	     z0 = svqrshl_s16_z (p0, z1, z4),
	     z0 = svqrshl_z (p0, z1, z4))

/*
** qrshl_w0_s16_z_tied1:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z0\.h
**	sqrshl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_s16_z_tied1, svint16_t, int16_t,
		 z0 = svqrshl_n_s16_z (p0, z0, x0),
		 z0 = svqrshl_z (p0, z0, x0))

/*
** qrshl_w0_s16_z_untied:
**	mov	(z[0-9]+\.h), w0
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	sqrshl	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	sqrshlr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_s16_z_untied, svint16_t, int16_t,
		 z0 = svqrshl_n_s16_z (p0, z1, x0),
		 z0 = svqrshl_z (p0, z1, x0))

/*
** qrshl_m16_s16_z:
**	movprfx	z0\.h, p0/z, z0\.h
**	srshr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (qrshl_m16_s16_z, svint16_t,
		z0 = svqrshl_n_s16_z (p0, z0, -16),
		z0 = svqrshl_z (p0, z0, -16))

/*
** qrshl_m2_s16_z:
**	movprfx	z0\.h, p0/z, z0\.h
**	srshr	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_m2_s16_z, svint16_t,
		z0 = svqrshl_n_s16_z (p0, z0, -2),
		z0 = svqrshl_z (p0, z0, -2))

/*
** qrshl_m1_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	srshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_s16_z_tied1, svint16_t,
		z0 = svqrshl_n_s16_z (p0, z0, -1),
		z0 = svqrshl_z (p0, z0, -1))

/*
** qrshl_m1_s16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	srshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_s16_z_untied, svint16_t,
		z0 = svqrshl_n_s16_z (p0, z1, -1),
		z0 = svqrshl_z (p0, z1, -1))

/*
** qrshl_1_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	sqshl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_s16_z_tied1, svint16_t,
		z0 = svqrshl_n_s16_z (p0, z0, 1),
		z0 = svqrshl_z (p0, z0, 1))

/*
** qrshl_1_s16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	sqshl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_s16_z_untied, svint16_t,
		z0 = svqrshl_n_s16_z (p0, z1, 1),
		z0 = svqrshl_z (p0, z1, 1))

/*
** qrshl_2_s16_z:
**	movprfx	z0\.h, p0/z, z0\.h
**	sqshl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_2_s16_z, svint16_t,
		z0 = svqrshl_n_s16_z (p0, z0, 2),
		z0 = svqrshl_z (p0, z0, 2))

/*
** qrshl_15_s16_z:
**	movprfx	z0\.h, p0/z, z0\.h
**	sqshl	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (qrshl_15_s16_z, svint16_t,
		z0 = svqrshl_n_s16_z (p0, z0, 15),
		z0 = svqrshl_z (p0, z0, 15))

/*
** qrshl_s16_x_tied1:
**	sqrshl	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (qrshl_s16_x_tied1, svint16_t, svint16_t,
	     z0 = svqrshl_s16_x (p0, z0, z4),
	     z0 = svqrshl_x (p0, z0, z4))

/*
** qrshl_s16_x_tied2:
**	sqrshlr	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z_REV (qrshl_s16_x_tied2, svint16_t, svint16_t,
		 z0_res = svqrshl_s16_x (p0, z4, z0),
		 z0_res = svqrshl_x (p0, z4, z0))

/*
** qrshl_s16_x_untied:
** (
**	movprfx	z0, z1
**	sqrshl	z0\.h, p0/m, z0\.h, z4\.h
** |
**	movprfx	z0, z4
**	sqrshlr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_DUAL_Z (qrshl_s16_x_untied, svint16_t, svint16_t,
	     z0 = svqrshl_s16_x (p0, z1, z4),
	     z0 = svqrshl_x (p0, z1, z4))

/*
** qrshl_w0_s16_x_tied1:
**	mov	(z[0-9]+\.h), w0
**	sqrshl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_s16_x_tied1, svint16_t, int16_t,
		 z0 = svqrshl_n_s16_x (p0, z0, x0),
		 z0 = svqrshl_x (p0, z0, x0))

/*
** qrshl_w0_s16_x_untied:
**	mov	z0\.h, w0
**	sqrshlr	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_s16_x_untied, svint16_t, int16_t,
		 z0 = svqrshl_n_s16_x (p0, z1, x0),
		 z0 = svqrshl_x (p0, z1, x0))

/*
** qrshl_m16_s16_x:
**	srshr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (qrshl_m16_s16_x, svint16_t,
		z0 = svqrshl_n_s16_x (p0, z0, -16),
		z0 = svqrshl_x (p0, z0, -16))

/*
** qrshl_m2_s16_x:
**	srshr	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_m2_s16_x, svint16_t,
		z0 = svqrshl_n_s16_x (p0, z0, -2),
		z0 = svqrshl_x (p0, z0, -2))

/*
** qrshl_m1_s16_x_tied1:
**	srshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_s16_x_tied1, svint16_t,
		z0 = svqrshl_n_s16_x (p0, z0, -1),
		z0 = svqrshl_x (p0, z0, -1))

/*
** qrshl_m1_s16_x_untied:
**	movprfx	z0, z1
**	srshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_s16_x_untied, svint16_t,
		z0 = svqrshl_n_s16_x (p0, z1, -1),
		z0 = svqrshl_x (p0, z1, -1))

/*
** qrshl_1_s16_x_tied1:
**	sqshl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_s16_x_tied1, svint16_t,
		z0 = svqrshl_n_s16_x (p0, z0, 1),
		z0 = svqrshl_x (p0, z0, 1))

/*
** qrshl_1_s16_x_untied:
**	movprfx	z0, z1
**	sqshl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_s16_x_untied, svint16_t,
		z0 = svqrshl_n_s16_x (p0, z1, 1),
		z0 = svqrshl_x (p0, z1, 1))

/*
** qrshl_2_s16_x:
**	sqshl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_2_s16_x, svint16_t,
		z0 = svqrshl_n_s16_x (p0, z0, 2),
		z0 = svqrshl_x (p0, z0, 2))

/*
** qrshl_15_s16_x:
**	sqshl	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (qrshl_15_s16_x, svint16_t,
		z0 = svqrshl_n_s16_x (p0, z0, 15),
		z0 = svqrshl_x (p0, z0, 15))
