/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrshl_s8_m_tied1:
**	sqrshl	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (qrshl_s8_m_tied1, svint8_t, svint8_t,
	     z0 = svqrshl_s8_m (p0, z0, z4),
	     z0 = svqrshl_m (p0, z0, z4))

/*
** qrshl_s8_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sqrshl	z0\.b, p0/m, z0\.b, \1\.b
**	ret
*/
TEST_DUAL_Z_REV (qrshl_s8_m_tied2, svint8_t, svint8_t,
		 z0_res = svqrshl_s8_m (p0, z4, z0),
		 z0_res = svqrshl_m (p0, z4, z0))

/*
** qrshl_s8_m_untied:
**	movprfx	z0, z1
**	sqrshl	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (qrshl_s8_m_untied, svint8_t, svint8_t,
	     z0 = svqrshl_s8_m (p0, z1, z4),
	     z0 = svqrshl_m (p0, z1, z4))

/*
** qrshl_w0_s8_m_tied1:
**	mov	(z[0-9]+\.b), w0
**	sqrshl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_s8_m_tied1, svint8_t, int8_t,
		 z0 = svqrshl_n_s8_m (p0, z0, x0),
		 z0 = svqrshl_m (p0, z0, x0))

/*
** qrshl_w0_s8_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	sqrshl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_s8_m_untied, svint8_t, int8_t,
		 z0 = svqrshl_n_s8_m (p0, z1, x0),
		 z0 = svqrshl_m (p0, z1, x0))

/*
** qrshl_m8_s8_m:
**	srshr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (qrshl_m8_s8_m, svint8_t,
		z0 = svqrshl_n_s8_m (p0, z0, -8),
		z0 = svqrshl_m (p0, z0, -8))

/*
** qrshl_m2_s8_m:
**	srshr	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_m2_s8_m, svint8_t,
		z0 = svqrshl_n_s8_m (p0, z0, -2),
		z0 = svqrshl_m (p0, z0, -2))

/*
** qrshl_m1_s8_m_tied1:
**	srshr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_s8_m_tied1, svint8_t,
		z0 = svqrshl_n_s8_m (p0, z0, -1),
		z0 = svqrshl_m (p0, z0, -1))

/*
** qrshl_m1_s8_m_untied:
**	movprfx	z0, z1
**	srshr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_s8_m_untied, svint8_t,
		z0 = svqrshl_n_s8_m (p0, z1, -1),
		z0 = svqrshl_m (p0, z1, -1))

/*
** qrshl_1_s8_m_tied1:
**	sqshl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_s8_m_tied1, svint8_t,
		z0 = svqrshl_n_s8_m (p0, z0, 1),
		z0 = svqrshl_m (p0, z0, 1))

/*
** qrshl_1_s8_m_untied:
**	movprfx	z0, z1
**	sqshl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_s8_m_untied, svint8_t,
		z0 = svqrshl_n_s8_m (p0, z1, 1),
		z0 = svqrshl_m (p0, z1, 1))

/*
** qrshl_2_s8_m:
**	sqshl	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_2_s8_m, svint8_t,
		z0 = svqrshl_n_s8_m (p0, z0, 2),
		z0 = svqrshl_m (p0, z0, 2))

/*
** qrshl_7_s8_m:
**	sqshl	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (qrshl_7_s8_m, svint8_t,
		z0 = svqrshl_n_s8_m (p0, z0, 7),
		z0 = svqrshl_m (p0, z0, 7))

/*
** qrshl_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	sqrshl	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (qrshl_s8_z_tied1, svint8_t, svint8_t,
	     z0 = svqrshl_s8_z (p0, z0, z4),
	     z0 = svqrshl_z (p0, z0, z4))

/*
** qrshl_s8_z_tied2:
**	movprfx	z0\.b, p0/z, z0\.b
**	sqrshlr	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z_REV (qrshl_s8_z_tied2, svint8_t, svint8_t,
		 z0_res = svqrshl_s8_z (p0, z4, z0),
		 z0_res = svqrshl_z (p0, z4, z0))

/*
** qrshl_s8_z_untied:
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	sqrshl	z0\.b, p0/m, z0\.b, z4\.b
** |
**	movprfx	z0\.b, p0/z, z4\.b
**	sqrshlr	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_DUAL_Z (qrshl_s8_z_untied, svint8_t, svint8_t,
	     z0 = svqrshl_s8_z (p0, z1, z4),
	     z0 = svqrshl_z (p0, z1, z4))

/*
** qrshl_w0_s8_z_tied1:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0\.b, p0/z, z0\.b
**	sqrshl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_s8_z_tied1, svint8_t, int8_t,
		 z0 = svqrshl_n_s8_z (p0, z0, x0),
		 z0 = svqrshl_z (p0, z0, x0))

/*
** qrshl_w0_s8_z_untied:
**	mov	(z[0-9]+\.b), w0
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	sqrshl	z0\.b, p0/m, z0\.b, \1
** |
**	movprfx	z0\.b, p0/z, \1
**	sqrshlr	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_s8_z_untied, svint8_t, int8_t,
		 z0 = svqrshl_n_s8_z (p0, z1, x0),
		 z0 = svqrshl_z (p0, z1, x0))

/*
** qrshl_m8_s8_z:
**	movprfx	z0\.b, p0/z, z0\.b
**	srshr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (qrshl_m8_s8_z, svint8_t,
		z0 = svqrshl_n_s8_z (p0, z0, -8),
		z0 = svqrshl_z (p0, z0, -8))

/*
** qrshl_m2_s8_z:
**	movprfx	z0\.b, p0/z, z0\.b
**	srshr	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_m2_s8_z, svint8_t,
		z0 = svqrshl_n_s8_z (p0, z0, -2),
		z0 = svqrshl_z (p0, z0, -2))

/*
** qrshl_m1_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	srshr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_s8_z_tied1, svint8_t,
		z0 = svqrshl_n_s8_z (p0, z0, -1),
		z0 = svqrshl_z (p0, z0, -1))

/*
** qrshl_m1_s8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	srshr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_s8_z_untied, svint8_t,
		z0 = svqrshl_n_s8_z (p0, z1, -1),
		z0 = svqrshl_z (p0, z1, -1))

/*
** qrshl_1_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	sqshl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_s8_z_tied1, svint8_t,
		z0 = svqrshl_n_s8_z (p0, z0, 1),
		z0 = svqrshl_z (p0, z0, 1))

/*
** qrshl_1_s8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	sqshl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_s8_z_untied, svint8_t,
		z0 = svqrshl_n_s8_z (p0, z1, 1),
		z0 = svqrshl_z (p0, z1, 1))

/*
** qrshl_2_s8_z:
**	movprfx	z0\.b, p0/z, z0\.b
**	sqshl	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_2_s8_z, svint8_t,
		z0 = svqrshl_n_s8_z (p0, z0, 2),
		z0 = svqrshl_z (p0, z0, 2))

/*
** qrshl_7_s8_z:
**	movprfx	z0\.b, p0/z, z0\.b
**	sqshl	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (qrshl_7_s8_z, svint8_t,
		z0 = svqrshl_n_s8_z (p0, z0, 7),
		z0 = svqrshl_z (p0, z0, 7))

/*
** qrshl_s8_x_tied1:
**	sqrshl	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (qrshl_s8_x_tied1, svint8_t, svint8_t,
	     z0 = svqrshl_s8_x (p0, z0, z4),
	     z0 = svqrshl_x (p0, z0, z4))

/*
** qrshl_s8_x_tied2:
**	sqrshlr	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z_REV (qrshl_s8_x_tied2, svint8_t, svint8_t,
		 z0_res = svqrshl_s8_x (p0, z4, z0),
		 z0_res = svqrshl_x (p0, z4, z0))

/*
** qrshl_s8_x_untied:
** (
**	movprfx	z0, z1
**	sqrshl	z0\.b, p0/m, z0\.b, z4\.b
** |
**	movprfx	z0, z4
**	sqrshlr	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_DUAL_Z (qrshl_s8_x_untied, svint8_t, svint8_t,
	     z0 = svqrshl_s8_x (p0, z1, z4),
	     z0 = svqrshl_x (p0, z1, z4))

/*
** qrshl_w0_s8_x_tied1:
**	mov	(z[0-9]+\.b), w0
**	sqrshl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_s8_x_tied1, svint8_t, int8_t,
		 z0 = svqrshl_n_s8_x (p0, z0, x0),
		 z0 = svqrshl_x (p0, z0, x0))

/*
** qrshl_w0_s8_x_untied:
**	mov	z0\.b, w0
**	sqrshlr	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_s8_x_untied, svint8_t, int8_t,
		 z0 = svqrshl_n_s8_x (p0, z1, x0),
		 z0 = svqrshl_x (p0, z1, x0))

/*
** qrshl_m8_s8_x:
**	srshr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (qrshl_m8_s8_x, svint8_t,
		z0 = svqrshl_n_s8_x (p0, z0, -8),
		z0 = svqrshl_x (p0, z0, -8))

/*
** qrshl_m2_s8_x:
**	srshr	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_m2_s8_x, svint8_t,
		z0 = svqrshl_n_s8_x (p0, z0, -2),
		z0 = svqrshl_x (p0, z0, -2))

/*
** qrshl_m1_s8_x_tied1:
**	srshr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_s8_x_tied1, svint8_t,
		z0 = svqrshl_n_s8_x (p0, z0, -1),
		z0 = svqrshl_x (p0, z0, -1))

/*
** qrshl_m1_s8_x_untied:
**	movprfx	z0, z1
**	srshr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_s8_x_untied, svint8_t,
		z0 = svqrshl_n_s8_x (p0, z1, -1),
		z0 = svqrshl_x (p0, z1, -1))

/*
** qrshl_1_s8_x_tied1:
**	sqshl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_s8_x_tied1, svint8_t,
		z0 = svqrshl_n_s8_x (p0, z0, 1),
		z0 = svqrshl_x (p0, z0, 1))

/*
** qrshl_1_s8_x_untied:
**	movprfx	z0, z1
**	sqshl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_s8_x_untied, svint8_t,
		z0 = svqrshl_n_s8_x (p0, z1, 1),
		z0 = svqrshl_x (p0, z1, 1))

/*
** qrshl_2_s8_x:
**	sqshl	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_2_s8_x, svint8_t,
		z0 = svqrshl_n_s8_x (p0, z0, 2),
		z0 = svqrshl_x (p0, z0, 2))

/*
** qrshl_7_s8_x:
**	sqshl	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (qrshl_7_s8_x, svint8_t,
		z0 = svqrshl_n_s8_x (p0, z0, 7),
		z0 = svqrshl_x (p0, z0, 7))
