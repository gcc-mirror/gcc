/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshl_u16_m_tied1:
**	uqshl	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (qshl_u16_m_tied1, svuint16_t, svint16_t,
	     z0 = svqshl_u16_m (p0, z0, z4),
	     z0 = svqshl_m (p0, z0, z4))

/*
** qshl_u16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	uqshl	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (qshl_u16_m_tied2, svuint16_t, svint16_t,
		 z0_res = svqshl_u16_m (p0, z4, z0),
		 z0_res = svqshl_m (p0, z4, z0))

/*
** qshl_u16_m_untied:
**	movprfx	z0, z1
**	uqshl	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (qshl_u16_m_untied, svuint16_t, svint16_t,
	     z0 = svqshl_u16_m (p0, z1, z4),
	     z0 = svqshl_m (p0, z1, z4))

/*
** qshl_w0_u16_m_tied1:
**	mov	(z[0-9]+\.h), w0
**	uqshl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_u16_m_tied1, svuint16_t, int16_t,
		 z0 = svqshl_n_u16_m (p0, z0, x0),
		 z0 = svqshl_m (p0, z0, x0))

/*
** qshl_w0_u16_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	uqshl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_u16_m_untied, svuint16_t, int16_t,
		 z0 = svqshl_n_u16_m (p0, z1, x0),
		 z0 = svqshl_m (p0, z1, x0))

/*
** qshl_m16_u16_m:
**	lsr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (qshl_m16_u16_m, svuint16_t,
		z0 = svqshl_n_u16_m (p0, z0, -16),
		z0 = svqshl_m (p0, z0, -16))

/*
** qshl_m2_u16_m:
**	lsr	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_m2_u16_m, svuint16_t,
		z0 = svqshl_n_u16_m (p0, z0, -2),
		z0 = svqshl_m (p0, z0, -2))

/*
** qshl_m1_u16_m_tied1:
**	lsr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_u16_m_tied1, svuint16_t,
		z0 = svqshl_n_u16_m (p0, z0, -1),
		z0 = svqshl_m (p0, z0, -1))

/*
** qshl_m1_u16_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_u16_m_untied, svuint16_t,
		z0 = svqshl_n_u16_m (p0, z1, -1),
		z0 = svqshl_m (p0, z1, -1))

/*
** qshl_1_u16_m_tied1:
**	uqshl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_u16_m_tied1, svuint16_t,
		z0 = svqshl_n_u16_m (p0, z0, 1),
		z0 = svqshl_m (p0, z0, 1))

/*
** qshl_1_u16_m_untied:
**	movprfx	z0, z1
**	uqshl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_u16_m_untied, svuint16_t,
		z0 = svqshl_n_u16_m (p0, z1, 1),
		z0 = svqshl_m (p0, z1, 1))

/*
** qshl_2_u16_m:
**	uqshl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_2_u16_m, svuint16_t,
		z0 = svqshl_n_u16_m (p0, z0, 2),
		z0 = svqshl_m (p0, z0, 2))

/*
** qshl_15_u16_m:
**	uqshl	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (qshl_15_u16_m, svuint16_t,
		z0 = svqshl_n_u16_m (p0, z0, 15),
		z0 = svqshl_m (p0, z0, 15))

/*
** qshl_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	uqshl	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (qshl_u16_z_tied1, svuint16_t, svint16_t,
	     z0 = svqshl_u16_z (p0, z0, z4),
	     z0 = svqshl_z (p0, z0, z4))

/*
** qshl_u16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	uqshlr	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z_REV (qshl_u16_z_tied2, svuint16_t, svint16_t,
		 z0_res = svqshl_u16_z (p0, z4, z0),
		 z0_res = svqshl_z (p0, z4, z0))

/*
** qshl_u16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	uqshl	z0\.h, p0/m, z0\.h, z4\.h
** |
**	movprfx	z0\.h, p0/z, z4\.h
**	uqshlr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_DUAL_Z (qshl_u16_z_untied, svuint16_t, svint16_t,
	     z0 = svqshl_u16_z (p0, z1, z4),
	     z0 = svqshl_z (p0, z1, z4))

/*
** qshl_w0_u16_z_tied1:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z0\.h
**	uqshl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_u16_z_tied1, svuint16_t, int16_t,
		 z0 = svqshl_n_u16_z (p0, z0, x0),
		 z0 = svqshl_z (p0, z0, x0))

/*
** qshl_w0_u16_z_untied:
**	mov	(z[0-9]+\.h), w0
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	uqshl	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	uqshlr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_u16_z_untied, svuint16_t, int16_t,
		 z0 = svqshl_n_u16_z (p0, z1, x0),
		 z0 = svqshl_z (p0, z1, x0))

/*
** qshl_m16_u16_z:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (qshl_m16_u16_z, svuint16_t,
		z0 = svqshl_n_u16_z (p0, z0, -16),
		z0 = svqshl_z (p0, z0, -16))

/*
** qshl_m2_u16_z:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsr	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_m2_u16_z, svuint16_t,
		z0 = svqshl_n_u16_z (p0, z0, -2),
		z0 = svqshl_z (p0, z0, -2))

/*
** qshl_m1_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_u16_z_tied1, svuint16_t,
		z0 = svqshl_n_u16_z (p0, z0, -1),
		z0 = svqshl_z (p0, z0, -1))

/*
** qshl_m1_u16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	lsr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_u16_z_untied, svuint16_t,
		z0 = svqshl_n_u16_z (p0, z1, -1),
		z0 = svqshl_z (p0, z1, -1))

/*
** qshl_1_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	uqshl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_u16_z_tied1, svuint16_t,
		z0 = svqshl_n_u16_z (p0, z0, 1),
		z0 = svqshl_z (p0, z0, 1))

/*
** qshl_1_u16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	uqshl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_u16_z_untied, svuint16_t,
		z0 = svqshl_n_u16_z (p0, z1, 1),
		z0 = svqshl_z (p0, z1, 1))

/*
** qshl_2_u16_z:
**	movprfx	z0\.h, p0/z, z0\.h
**	uqshl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_2_u16_z, svuint16_t,
		z0 = svqshl_n_u16_z (p0, z0, 2),
		z0 = svqshl_z (p0, z0, 2))

/*
** qshl_15_u16_z:
**	movprfx	z0\.h, p0/z, z0\.h
**	uqshl	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (qshl_15_u16_z, svuint16_t,
		z0 = svqshl_n_u16_z (p0, z0, 15),
		z0 = svqshl_z (p0, z0, 15))

/*
** qshl_u16_x_tied1:
**	uqshl	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (qshl_u16_x_tied1, svuint16_t, svint16_t,
	     z0 = svqshl_u16_x (p0, z0, z4),
	     z0 = svqshl_x (p0, z0, z4))

/*
** qshl_u16_x_tied2:
**	uqshlr	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z_REV (qshl_u16_x_tied2, svuint16_t, svint16_t,
		 z0_res = svqshl_u16_x (p0, z4, z0),
		 z0_res = svqshl_x (p0, z4, z0))

/*
** qshl_u16_x_untied:
** (
**	movprfx	z0, z1
**	uqshl	z0\.h, p0/m, z0\.h, z4\.h
** |
**	movprfx	z0, z4
**	uqshlr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_DUAL_Z (qshl_u16_x_untied, svuint16_t, svint16_t,
	     z0 = svqshl_u16_x (p0, z1, z4),
	     z0 = svqshl_x (p0, z1, z4))

/*
** qshl_w0_u16_x_tied1:
**	mov	(z[0-9]+\.h), w0
**	uqshl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_u16_x_tied1, svuint16_t, int16_t,
		 z0 = svqshl_n_u16_x (p0, z0, x0),
		 z0 = svqshl_x (p0, z0, x0))

/*
** qshl_w0_u16_x_untied:
**	mov	z0\.h, w0
**	uqshlr	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_u16_x_untied, svuint16_t, int16_t,
		 z0 = svqshl_n_u16_x (p0, z1, x0),
		 z0 = svqshl_x (p0, z1, x0))

/*
** qshl_m16_u16_x:
**	lsr	z0\.h, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (qshl_m16_u16_x, svuint16_t,
		z0 = svqshl_n_u16_x (p0, z0, -16),
		z0 = svqshl_x (p0, z0, -16))

/*
** qshl_m2_u16_x:
**	lsr	z0\.h, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_m2_u16_x, svuint16_t,
		z0 = svqshl_n_u16_x (p0, z0, -2),
		z0 = svqshl_x (p0, z0, -2))

/*
** qshl_m1_u16_x_tied1:
**	lsr	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_u16_x_tied1, svuint16_t,
		z0 = svqshl_n_u16_x (p0, z0, -1),
		z0 = svqshl_x (p0, z0, -1))

/*
** qshl_m1_u16_x_untied:
**	lsr	z0\.h, z1\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_u16_x_untied, svuint16_t,
		z0 = svqshl_n_u16_x (p0, z1, -1),
		z0 = svqshl_x (p0, z1, -1))

/*
** qshl_1_u16_x_tied1:
**	uqshl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_u16_x_tied1, svuint16_t,
		z0 = svqshl_n_u16_x (p0, z0, 1),
		z0 = svqshl_x (p0, z0, 1))

/*
** qshl_1_u16_x_untied:
**	movprfx	z0, z1
**	uqshl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_u16_x_untied, svuint16_t,
		z0 = svqshl_n_u16_x (p0, z1, 1),
		z0 = svqshl_x (p0, z1, 1))

/*
** qshl_2_u16_x:
**	uqshl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_2_u16_x, svuint16_t,
		z0 = svqshl_n_u16_x (p0, z0, 2),
		z0 = svqshl_x (p0, z0, 2))

/*
** qshl_15_u16_x:
**	uqshl	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (qshl_15_u16_x, svuint16_t,
		z0 = svqshl_n_u16_x (p0, z0, 15),
		z0 = svqshl_x (p0, z0, 15))
