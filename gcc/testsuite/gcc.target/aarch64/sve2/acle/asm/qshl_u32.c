/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshl_u32_m_tied1:
**	uqshl	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (qshl_u32_m_tied1, svuint32_t, svint32_t,
	     z0 = svqshl_u32_m (p0, z0, z4),
	     z0 = svqshl_m (p0, z0, z4))

/*
** qshl_u32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	uqshl	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (qshl_u32_m_tied2, svuint32_t, svint32_t,
		 z0_res = svqshl_u32_m (p0, z4, z0),
		 z0_res = svqshl_m (p0, z4, z0))

/*
** qshl_u32_m_untied:
**	movprfx	z0, z1
**	uqshl	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (qshl_u32_m_untied, svuint32_t, svint32_t,
	     z0 = svqshl_u32_m (p0, z1, z4),
	     z0 = svqshl_m (p0, z1, z4))

/*
** qshl_w0_u32_m_tied1:
**	mov	(z[0-9]+\.s), w0
**	uqshl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_u32_m_tied1, svuint32_t, int32_t,
		 z0 = svqshl_n_u32_m (p0, z0, x0),
		 z0 = svqshl_m (p0, z0, x0))

/*
** qshl_w0_u32_m_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	uqshl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_u32_m_untied, svuint32_t, int32_t,
		 z0 = svqshl_n_u32_m (p0, z1, x0),
		 z0 = svqshl_m (p0, z1, x0))

/*
** qshl_m32_u32_m:
**	lsr	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (qshl_m32_u32_m, svuint32_t,
		z0 = svqshl_n_u32_m (p0, z0, -32),
		z0 = svqshl_m (p0, z0, -32))

/*
** qshl_m2_u32_m:
**	lsr	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_m2_u32_m, svuint32_t,
		z0 = svqshl_n_u32_m (p0, z0, -2),
		z0 = svqshl_m (p0, z0, -2))

/*
** qshl_m1_u32_m_tied1:
**	lsr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_u32_m_tied1, svuint32_t,
		z0 = svqshl_n_u32_m (p0, z0, -1),
		z0 = svqshl_m (p0, z0, -1))

/*
** qshl_m1_u32_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_u32_m_untied, svuint32_t,
		z0 = svqshl_n_u32_m (p0, z1, -1),
		z0 = svqshl_m (p0, z1, -1))

/*
** qshl_1_u32_m_tied1:
**	uqshl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_u32_m_tied1, svuint32_t,
		z0 = svqshl_n_u32_m (p0, z0, 1),
		z0 = svqshl_m (p0, z0, 1))

/*
** qshl_1_u32_m_untied:
**	movprfx	z0, z1
**	uqshl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_u32_m_untied, svuint32_t,
		z0 = svqshl_n_u32_m (p0, z1, 1),
		z0 = svqshl_m (p0, z1, 1))

/*
** qshl_2_u32_m:
**	uqshl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_2_u32_m, svuint32_t,
		z0 = svqshl_n_u32_m (p0, z0, 2),
		z0 = svqshl_m (p0, z0, 2))

/*
** qshl_31_u32_m:
**	uqshl	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (qshl_31_u32_m, svuint32_t,
		z0 = svqshl_n_u32_m (p0, z0, 31),
		z0 = svqshl_m (p0, z0, 31))

/*
** qshl_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	uqshl	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (qshl_u32_z_tied1, svuint32_t, svint32_t,
	     z0 = svqshl_u32_z (p0, z0, z4),
	     z0 = svqshl_z (p0, z0, z4))

/*
** qshl_u32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	uqshlr	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z_REV (qshl_u32_z_tied2, svuint32_t, svint32_t,
		 z0_res = svqshl_u32_z (p0, z4, z0),
		 z0_res = svqshl_z (p0, z4, z0))

/*
** qshl_u32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	uqshl	z0\.s, p0/m, z0\.s, z4\.s
** |
**	movprfx	z0\.s, p0/z, z4\.s
**	uqshlr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_DUAL_Z (qshl_u32_z_untied, svuint32_t, svint32_t,
	     z0 = svqshl_u32_z (p0, z1, z4),
	     z0 = svqshl_z (p0, z1, z4))

/*
** qshl_w0_u32_z_tied1:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	uqshl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_u32_z_tied1, svuint32_t, int32_t,
		 z0 = svqshl_n_u32_z (p0, z0, x0),
		 z0 = svqshl_z (p0, z0, x0))

/*
** qshl_w0_u32_z_untied:
**	mov	(z[0-9]+\.s), w0
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	uqshl	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	uqshlr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_u32_z_untied, svuint32_t, int32_t,
		 z0 = svqshl_n_u32_z (p0, z1, x0),
		 z0 = svqshl_z (p0, z1, x0))

/*
** qshl_m32_u32_z:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsr	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (qshl_m32_u32_z, svuint32_t,
		z0 = svqshl_n_u32_z (p0, z0, -32),
		z0 = svqshl_z (p0, z0, -32))

/*
** qshl_m2_u32_z:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsr	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_m2_u32_z, svuint32_t,
		z0 = svqshl_n_u32_z (p0, z0, -2),
		z0 = svqshl_z (p0, z0, -2))

/*
** qshl_m1_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_u32_z_tied1, svuint32_t,
		z0 = svqshl_n_u32_z (p0, z0, -1),
		z0 = svqshl_z (p0, z0, -1))

/*
** qshl_m1_u32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	lsr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_u32_z_untied, svuint32_t,
		z0 = svqshl_n_u32_z (p0, z1, -1),
		z0 = svqshl_z (p0, z1, -1))

/*
** qshl_1_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	uqshl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_u32_z_tied1, svuint32_t,
		z0 = svqshl_n_u32_z (p0, z0, 1),
		z0 = svqshl_z (p0, z0, 1))

/*
** qshl_1_u32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	uqshl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_u32_z_untied, svuint32_t,
		z0 = svqshl_n_u32_z (p0, z1, 1),
		z0 = svqshl_z (p0, z1, 1))

/*
** qshl_2_u32_z:
**	movprfx	z0\.s, p0/z, z0\.s
**	uqshl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_2_u32_z, svuint32_t,
		z0 = svqshl_n_u32_z (p0, z0, 2),
		z0 = svqshl_z (p0, z0, 2))

/*
** qshl_31_u32_z:
**	movprfx	z0\.s, p0/z, z0\.s
**	uqshl	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (qshl_31_u32_z, svuint32_t,
		z0 = svqshl_n_u32_z (p0, z0, 31),
		z0 = svqshl_z (p0, z0, 31))

/*
** qshl_u32_x_tied1:
**	uqshl	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (qshl_u32_x_tied1, svuint32_t, svint32_t,
	     z0 = svqshl_u32_x (p0, z0, z4),
	     z0 = svqshl_x (p0, z0, z4))

/*
** qshl_u32_x_tied2:
**	uqshlr	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z_REV (qshl_u32_x_tied2, svuint32_t, svint32_t,
		 z0_res = svqshl_u32_x (p0, z4, z0),
		 z0_res = svqshl_x (p0, z4, z0))

/*
** qshl_u32_x_untied:
** (
**	movprfx	z0, z1
**	uqshl	z0\.s, p0/m, z0\.s, z4\.s
** |
**	movprfx	z0, z4
**	uqshlr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_DUAL_Z (qshl_u32_x_untied, svuint32_t, svint32_t,
	     z0 = svqshl_u32_x (p0, z1, z4),
	     z0 = svqshl_x (p0, z1, z4))

/*
** qshl_w0_u32_x_tied1:
**	mov	(z[0-9]+\.s), w0
**	uqshl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_u32_x_tied1, svuint32_t, int32_t,
		 z0 = svqshl_n_u32_x (p0, z0, x0),
		 z0 = svqshl_x (p0, z0, x0))

/*
** qshl_w0_u32_x_untied:
**	mov	z0\.s, w0
**	uqshlr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZX (qshl_w0_u32_x_untied, svuint32_t, int32_t,
		 z0 = svqshl_n_u32_x (p0, z1, x0),
		 z0 = svqshl_x (p0, z1, x0))

/*
** qshl_m32_u32_x:
**	lsr	z0\.s, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (qshl_m32_u32_x, svuint32_t,
		z0 = svqshl_n_u32_x (p0, z0, -32),
		z0 = svqshl_x (p0, z0, -32))

/*
** qshl_m2_u32_x:
**	lsr	z0\.s, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_m2_u32_x, svuint32_t,
		z0 = svqshl_n_u32_x (p0, z0, -2),
		z0 = svqshl_x (p0, z0, -2))

/*
** qshl_m1_u32_x_tied1:
**	lsr	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_u32_x_tied1, svuint32_t,
		z0 = svqshl_n_u32_x (p0, z0, -1),
		z0 = svqshl_x (p0, z0, -1))

/*
** qshl_m1_u32_x_untied:
**	lsr	z0\.s, z1\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_u32_x_untied, svuint32_t,
		z0 = svqshl_n_u32_x (p0, z1, -1),
		z0 = svqshl_x (p0, z1, -1))

/*
** qshl_1_u32_x_tied1:
**	uqshl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_u32_x_tied1, svuint32_t,
		z0 = svqshl_n_u32_x (p0, z0, 1),
		z0 = svqshl_x (p0, z0, 1))

/*
** qshl_1_u32_x_untied:
**	movprfx	z0, z1
**	uqshl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_u32_x_untied, svuint32_t,
		z0 = svqshl_n_u32_x (p0, z1, 1),
		z0 = svqshl_x (p0, z1, 1))

/*
** qshl_2_u32_x:
**	uqshl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_2_u32_x, svuint32_t,
		z0 = svqshl_n_u32_x (p0, z0, 2),
		z0 = svqshl_x (p0, z0, 2))

/*
** qshl_31_u32_x:
**	uqshl	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (qshl_31_u32_x, svuint32_t,
		z0 = svqshl_n_u32_x (p0, z0, 31),
		z0 = svqshl_x (p0, z0, 31))
