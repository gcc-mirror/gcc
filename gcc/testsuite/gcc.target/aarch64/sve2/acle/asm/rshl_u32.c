/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rshl_u32_m_tied1:
**	urshl	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (rshl_u32_m_tied1, svuint32_t, svint32_t,
	     z0 = svrshl_u32_m (p0, z0, z4),
	     z0 = svrshl_m (p0, z0, z4))

/*
** rshl_u32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	urshl	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (rshl_u32_m_tied2, svuint32_t, svint32_t,
		 z0_res = svrshl_u32_m (p0, z4, z0),
		 z0_res = svrshl_m (p0, z4, z0))

/*
** rshl_u32_m_untied:
**	movprfx	z0, z1
**	urshl	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (rshl_u32_m_untied, svuint32_t, svint32_t,
	     z0 = svrshl_u32_m (p0, z1, z4),
	     z0 = svrshl_m (p0, z1, z4))

/*
** rshl_w0_u32_m_tied1:
**	mov	(z[0-9]+\.s), w0
**	urshl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_u32_m_tied1, svuint32_t, int32_t,
		 z0 = svrshl_n_u32_m (p0, z0, x0),
		 z0 = svrshl_m (p0, z0, x0))

/*
** rshl_w0_u32_m_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	urshl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_u32_m_untied, svuint32_t, int32_t,
		 z0 = svrshl_n_u32_m (p0, z1, x0),
		 z0 = svrshl_m (p0, z1, x0))

/*
** rshl_m32_u32_m:
**	urshr	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (rshl_m32_u32_m, svuint32_t,
		z0 = svrshl_n_u32_m (p0, z0, -32),
		z0 = svrshl_m (p0, z0, -32))

/*
** rshl_m2_u32_m:
**	urshr	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_m2_u32_m, svuint32_t,
		z0 = svrshl_n_u32_m (p0, z0, -2),
		z0 = svrshl_m (p0, z0, -2))

/*
** rshl_m1_u32_m_tied1:
**	urshr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_u32_m_tied1, svuint32_t,
		z0 = svrshl_n_u32_m (p0, z0, -1),
		z0 = svrshl_m (p0, z0, -1))

/*
** rshl_m1_u32_m_untied:
**	movprfx	z0, z1
**	urshr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_u32_m_untied, svuint32_t,
		z0 = svrshl_n_u32_m (p0, z1, -1),
		z0 = svrshl_m (p0, z1, -1))

/*
** rshl_1_u32_m_tied1:
**	lsl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_u32_m_tied1, svuint32_t,
		z0 = svrshl_n_u32_m (p0, z0, 1),
		z0 = svrshl_m (p0, z0, 1))

/*
** rshl_1_u32_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_u32_m_untied, svuint32_t,
		z0 = svrshl_n_u32_m (p0, z1, 1),
		z0 = svrshl_m (p0, z1, 1))

/*
** rshl_2_u32_m:
**	lsl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_2_u32_m, svuint32_t,
		z0 = svrshl_n_u32_m (p0, z0, 2),
		z0 = svrshl_m (p0, z0, 2))

/*
** rshl_31_u32_m:
**	lsl	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (rshl_31_u32_m, svuint32_t,
		z0 = svrshl_n_u32_m (p0, z0, 31),
		z0 = svrshl_m (p0, z0, 31))

/*
** rshl_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	urshl	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (rshl_u32_z_tied1, svuint32_t, svint32_t,
	     z0 = svrshl_u32_z (p0, z0, z4),
	     z0 = svrshl_z (p0, z0, z4))

/*
** rshl_u32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	urshlr	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z_REV (rshl_u32_z_tied2, svuint32_t, svint32_t,
		 z0_res = svrshl_u32_z (p0, z4, z0),
		 z0_res = svrshl_z (p0, z4, z0))

/*
** rshl_u32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	urshl	z0\.s, p0/m, z0\.s, z4\.s
** |
**	movprfx	z0\.s, p0/z, z4\.s
**	urshlr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_DUAL_Z (rshl_u32_z_untied, svuint32_t, svint32_t,
	     z0 = svrshl_u32_z (p0, z1, z4),
	     z0 = svrshl_z (p0, z1, z4))

/*
** rshl_w0_u32_z_tied1:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	urshl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_u32_z_tied1, svuint32_t, int32_t,
		 z0 = svrshl_n_u32_z (p0, z0, x0),
		 z0 = svrshl_z (p0, z0, x0))

/*
** rshl_w0_u32_z_untied:
**	mov	(z[0-9]+\.s), w0
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	urshl	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	urshlr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_u32_z_untied, svuint32_t, int32_t,
		 z0 = svrshl_n_u32_z (p0, z1, x0),
		 z0 = svrshl_z (p0, z1, x0))

/*
** rshl_m32_u32_z:
**	movprfx	z0\.s, p0/z, z0\.s
**	urshr	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (rshl_m32_u32_z, svuint32_t,
		z0 = svrshl_n_u32_z (p0, z0, -32),
		z0 = svrshl_z (p0, z0, -32))

/*
** rshl_m2_u32_z:
**	movprfx	z0\.s, p0/z, z0\.s
**	urshr	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_m2_u32_z, svuint32_t,
		z0 = svrshl_n_u32_z (p0, z0, -2),
		z0 = svrshl_z (p0, z0, -2))

/*
** rshl_m1_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	urshr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_u32_z_tied1, svuint32_t,
		z0 = svrshl_n_u32_z (p0, z0, -1),
		z0 = svrshl_z (p0, z0, -1))

/*
** rshl_m1_u32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	urshr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_u32_z_untied, svuint32_t,
		z0 = svrshl_n_u32_z (p0, z1, -1),
		z0 = svrshl_z (p0, z1, -1))

/*
** rshl_1_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_u32_z_tied1, svuint32_t,
		z0 = svrshl_n_u32_z (p0, z0, 1),
		z0 = svrshl_z (p0, z0, 1))

/*
** rshl_1_u32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	lsl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_u32_z_untied, svuint32_t,
		z0 = svrshl_n_u32_z (p0, z1, 1),
		z0 = svrshl_z (p0, z1, 1))

/*
** rshl_2_u32_z:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_2_u32_z, svuint32_t,
		z0 = svrshl_n_u32_z (p0, z0, 2),
		z0 = svrshl_z (p0, z0, 2))

/*
** rshl_31_u32_z:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsl	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (rshl_31_u32_z, svuint32_t,
		z0 = svrshl_n_u32_z (p0, z0, 31),
		z0 = svrshl_z (p0, z0, 31))

/*
** rshl_u32_x_tied1:
**	urshl	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (rshl_u32_x_tied1, svuint32_t, svint32_t,
	     z0 = svrshl_u32_x (p0, z0, z4),
	     z0 = svrshl_x (p0, z0, z4))

/*
** rshl_u32_x_tied2:
**	urshlr	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z_REV (rshl_u32_x_tied2, svuint32_t, svint32_t,
		 z0_res = svrshl_u32_x (p0, z4, z0),
		 z0_res = svrshl_x (p0, z4, z0))

/*
** rshl_u32_x_untied:
** (
**	movprfx	z0, z1
**	urshl	z0\.s, p0/m, z0\.s, z4\.s
** |
**	movprfx	z0, z4
**	urshlr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_DUAL_Z (rshl_u32_x_untied, svuint32_t, svint32_t,
	     z0 = svrshl_u32_x (p0, z1, z4),
	     z0 = svrshl_x (p0, z1, z4))

/*
** rshl_w0_u32_x_tied1:
**	mov	(z[0-9]+\.s), w0
**	urshl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_u32_x_tied1, svuint32_t, int32_t,
		 z0 = svrshl_n_u32_x (p0, z0, x0),
		 z0 = svrshl_x (p0, z0, x0))

/*
** rshl_w0_u32_x_untied:
**	mov	z0\.s, w0
**	urshlr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_u32_x_untied, svuint32_t, int32_t,
		 z0 = svrshl_n_u32_x (p0, z1, x0),
		 z0 = svrshl_x (p0, z1, x0))

/*
** rshl_m32_u32_x:
**	urshr	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (rshl_m32_u32_x, svuint32_t,
		z0 = svrshl_n_u32_x (p0, z0, -32),
		z0 = svrshl_x (p0, z0, -32))

/*
** rshl_m2_u32_x:
**	urshr	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_m2_u32_x, svuint32_t,
		z0 = svrshl_n_u32_x (p0, z0, -2),
		z0 = svrshl_x (p0, z0, -2))

/*
** rshl_m1_u32_x_tied1:
**	urshr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_u32_x_tied1, svuint32_t,
		z0 = svrshl_n_u32_x (p0, z0, -1),
		z0 = svrshl_x (p0, z0, -1))

/*
** rshl_m1_u32_x_untied:
**	movprfx	z0, z1
**	urshr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_u32_x_untied, svuint32_t,
		z0 = svrshl_n_u32_x (p0, z1, -1),
		z0 = svrshl_x (p0, z1, -1))

/*
** rshl_1_u32_x_tied1:
**	add	z0\.s, z0\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (rshl_1_u32_x_tied1, svuint32_t,
		z0 = svrshl_n_u32_x (p0, z0, 1),
		z0 = svrshl_x (p0, z0, 1))

/*
** rshl_1_u32_x_untied:
**	add	z0\.s, z1\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (rshl_1_u32_x_untied, svuint32_t,
		z0 = svrshl_n_u32_x (p0, z1, 1),
		z0 = svrshl_x (p0, z1, 1))

/*
** rshl_2_u32_x:
**	lsl	z0\.s, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_2_u32_x, svuint32_t,
		z0 = svrshl_n_u32_x (p0, z0, 2),
		z0 = svrshl_x (p0, z0, 2))

/*
** rshl_31_u32_x:
**	lsl	z0\.s, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (rshl_31_u32_x, svuint32_t,
		z0 = svrshl_n_u32_x (p0, z0, 31),
		z0 = svrshl_x (p0, z0, 31))
