/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrshl_u32_m_tied1:
**	uqrshl	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (qrshl_u32_m_tied1, svuint32_t, svint32_t,
	     z0 = svqrshl_u32_m (p0, z0, z4),
	     z0 = svqrshl_m (p0, z0, z4))

/*
** qrshl_u32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	uqrshl	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (qrshl_u32_m_tied2, svuint32_t, svint32_t,
		 z0_res = svqrshl_u32_m (p0, z4, z0),
		 z0_res = svqrshl_m (p0, z4, z0))

/*
** qrshl_u32_m_untied:
**	movprfx	z0, z1
**	uqrshl	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (qrshl_u32_m_untied, svuint32_t, svint32_t,
	     z0 = svqrshl_u32_m (p0, z1, z4),
	     z0 = svqrshl_m (p0, z1, z4))

/*
** qrshl_w0_u32_m_tied1:
**	mov	(z[0-9]+\.s), w0
**	uqrshl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_u32_m_tied1, svuint32_t, int32_t,
		 z0 = svqrshl_n_u32_m (p0, z0, x0),
		 z0 = svqrshl_m (p0, z0, x0))

/*
** qrshl_w0_u32_m_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	uqrshl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_u32_m_untied, svuint32_t, int32_t,
		 z0 = svqrshl_n_u32_m (p0, z1, x0),
		 z0 = svqrshl_m (p0, z1, x0))

/*
** qrshl_m32_u32_m:
**	urshr	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (qrshl_m32_u32_m, svuint32_t,
		z0 = svqrshl_n_u32_m (p0, z0, -32),
		z0 = svqrshl_m (p0, z0, -32))

/*
** qrshl_m2_u32_m:
**	urshr	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_m2_u32_m, svuint32_t,
		z0 = svqrshl_n_u32_m (p0, z0, -2),
		z0 = svqrshl_m (p0, z0, -2))

/*
** qrshl_m1_u32_m_tied1:
**	urshr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_u32_m_tied1, svuint32_t,
		z0 = svqrshl_n_u32_m (p0, z0, -1),
		z0 = svqrshl_m (p0, z0, -1))

/*
** qrshl_m1_u32_m_untied:
**	movprfx	z0, z1
**	urshr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_u32_m_untied, svuint32_t,
		z0 = svqrshl_n_u32_m (p0, z1, -1),
		z0 = svqrshl_m (p0, z1, -1))

/*
** qrshl_1_u32_m_tied1:
**	uqshl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_u32_m_tied1, svuint32_t,
		z0 = svqrshl_n_u32_m (p0, z0, 1),
		z0 = svqrshl_m (p0, z0, 1))

/*
** qrshl_1_u32_m_untied:
**	movprfx	z0, z1
**	uqshl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_u32_m_untied, svuint32_t,
		z0 = svqrshl_n_u32_m (p0, z1, 1),
		z0 = svqrshl_m (p0, z1, 1))

/*
** qrshl_2_u32_m:
**	uqshl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_2_u32_m, svuint32_t,
		z0 = svqrshl_n_u32_m (p0, z0, 2),
		z0 = svqrshl_m (p0, z0, 2))

/*
** qrshl_31_u32_m:
**	uqshl	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (qrshl_31_u32_m, svuint32_t,
		z0 = svqrshl_n_u32_m (p0, z0, 31),
		z0 = svqrshl_m (p0, z0, 31))

/*
** qrshl_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	uqrshl	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (qrshl_u32_z_tied1, svuint32_t, svint32_t,
	     z0 = svqrshl_u32_z (p0, z0, z4),
	     z0 = svqrshl_z (p0, z0, z4))

/*
** qrshl_u32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	uqrshlr	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z_REV (qrshl_u32_z_tied2, svuint32_t, svint32_t,
		 z0_res = svqrshl_u32_z (p0, z4, z0),
		 z0_res = svqrshl_z (p0, z4, z0))

/*
** qrshl_u32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	uqrshl	z0\.s, p0/m, z0\.s, z4\.s
** |
**	movprfx	z0\.s, p0/z, z4\.s
**	uqrshlr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_DUAL_Z (qrshl_u32_z_untied, svuint32_t, svint32_t,
	     z0 = svqrshl_u32_z (p0, z1, z4),
	     z0 = svqrshl_z (p0, z1, z4))

/*
** qrshl_w0_u32_z_tied1:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	uqrshl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_u32_z_tied1, svuint32_t, int32_t,
		 z0 = svqrshl_n_u32_z (p0, z0, x0),
		 z0 = svqrshl_z (p0, z0, x0))

/*
** qrshl_w0_u32_z_untied:
**	mov	(z[0-9]+\.s), w0
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	uqrshl	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	uqrshlr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_u32_z_untied, svuint32_t, int32_t,
		 z0 = svqrshl_n_u32_z (p0, z1, x0),
		 z0 = svqrshl_z (p0, z1, x0))

/*
** qrshl_m32_u32_z:
**	movprfx	z0\.s, p0/z, z0\.s
**	urshr	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (qrshl_m32_u32_z, svuint32_t,
		z0 = svqrshl_n_u32_z (p0, z0, -32),
		z0 = svqrshl_z (p0, z0, -32))

/*
** qrshl_m2_u32_z:
**	movprfx	z0\.s, p0/z, z0\.s
**	urshr	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_m2_u32_z, svuint32_t,
		z0 = svqrshl_n_u32_z (p0, z0, -2),
		z0 = svqrshl_z (p0, z0, -2))

/*
** qrshl_m1_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	urshr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_u32_z_tied1, svuint32_t,
		z0 = svqrshl_n_u32_z (p0, z0, -1),
		z0 = svqrshl_z (p0, z0, -1))

/*
** qrshl_m1_u32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	urshr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_u32_z_untied, svuint32_t,
		z0 = svqrshl_n_u32_z (p0, z1, -1),
		z0 = svqrshl_z (p0, z1, -1))

/*
** qrshl_1_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	uqshl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_u32_z_tied1, svuint32_t,
		z0 = svqrshl_n_u32_z (p0, z0, 1),
		z0 = svqrshl_z (p0, z0, 1))

/*
** qrshl_1_u32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	uqshl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_u32_z_untied, svuint32_t,
		z0 = svqrshl_n_u32_z (p0, z1, 1),
		z0 = svqrshl_z (p0, z1, 1))

/*
** qrshl_2_u32_z:
**	movprfx	z0\.s, p0/z, z0\.s
**	uqshl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_2_u32_z, svuint32_t,
		z0 = svqrshl_n_u32_z (p0, z0, 2),
		z0 = svqrshl_z (p0, z0, 2))

/*
** qrshl_31_u32_z:
**	movprfx	z0\.s, p0/z, z0\.s
**	uqshl	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (qrshl_31_u32_z, svuint32_t,
		z0 = svqrshl_n_u32_z (p0, z0, 31),
		z0 = svqrshl_z (p0, z0, 31))

/*
** qrshl_u32_x_tied1:
**	uqrshl	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (qrshl_u32_x_tied1, svuint32_t, svint32_t,
	     z0 = svqrshl_u32_x (p0, z0, z4),
	     z0 = svqrshl_x (p0, z0, z4))

/*
** qrshl_u32_x_tied2:
**	uqrshlr	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z_REV (qrshl_u32_x_tied2, svuint32_t, svint32_t,
		 z0_res = svqrshl_u32_x (p0, z4, z0),
		 z0_res = svqrshl_x (p0, z4, z0))

/*
** qrshl_u32_x_untied:
** (
**	movprfx	z0, z1
**	uqrshl	z0\.s, p0/m, z0\.s, z4\.s
** |
**	movprfx	z0, z4
**	uqrshlr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_DUAL_Z (qrshl_u32_x_untied, svuint32_t, svint32_t,
	     z0 = svqrshl_u32_x (p0, z1, z4),
	     z0 = svqrshl_x (p0, z1, z4))

/*
** qrshl_w0_u32_x_tied1:
**	mov	(z[0-9]+\.s), w0
**	uqrshl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_u32_x_tied1, svuint32_t, int32_t,
		 z0 = svqrshl_n_u32_x (p0, z0, x0),
		 z0 = svqrshl_x (p0, z0, x0))

/*
** qrshl_w0_u32_x_untied:
**	mov	z0\.s, w0
**	uqrshlr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZX (qrshl_w0_u32_x_untied, svuint32_t, int32_t,
		 z0 = svqrshl_n_u32_x (p0, z1, x0),
		 z0 = svqrshl_x (p0, z1, x0))

/*
** qrshl_m32_u32_x:
**	urshr	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (qrshl_m32_u32_x, svuint32_t,
		z0 = svqrshl_n_u32_x (p0, z0, -32),
		z0 = svqrshl_x (p0, z0, -32))

/*
** qrshl_m2_u32_x:
**	urshr	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_m2_u32_x, svuint32_t,
		z0 = svqrshl_n_u32_x (p0, z0, -2),
		z0 = svqrshl_x (p0, z0, -2))

/*
** qrshl_m1_u32_x_tied1:
**	urshr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_u32_x_tied1, svuint32_t,
		z0 = svqrshl_n_u32_x (p0, z0, -1),
		z0 = svqrshl_x (p0, z0, -1))

/*
** qrshl_m1_u32_x_untied:
**	movprfx	z0, z1
**	urshr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_u32_x_untied, svuint32_t,
		z0 = svqrshl_n_u32_x (p0, z1, -1),
		z0 = svqrshl_x (p0, z1, -1))

/*
** qrshl_1_u32_x_tied1:
**	uqshl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_u32_x_tied1, svuint32_t,
		z0 = svqrshl_n_u32_x (p0, z0, 1),
		z0 = svqrshl_x (p0, z0, 1))

/*
** qrshl_1_u32_x_untied:
**	movprfx	z0, z1
**	uqshl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_u32_x_untied, svuint32_t,
		z0 = svqrshl_n_u32_x (p0, z1, 1),
		z0 = svqrshl_x (p0, z1, 1))

/*
** qrshl_2_u32_x:
**	uqshl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_2_u32_x, svuint32_t,
		z0 = svqrshl_n_u32_x (p0, z0, 2),
		z0 = svqrshl_x (p0, z0, 2))

/*
** qrshl_31_u32_x:
**	uqshl	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (qrshl_31_u32_x, svuint32_t,
		z0 = svqrshl_n_u32_x (p0, z0, 31),
		z0 = svqrshl_x (p0, z0, 31))
