/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rshl_s32_m_tied1:
**	srshl	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (rshl_s32_m_tied1, svint32_t, svint32_t,
	     z0 = svrshl_s32_m (p0, z0, z4),
	     z0 = svrshl_m (p0, z0, z4))

/*
** rshl_s32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	srshl	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (rshl_s32_m_tied2, svint32_t, svint32_t,
		 z0_res = svrshl_s32_m (p0, z4, z0),
		 z0_res = svrshl_m (p0, z4, z0))

/*
** rshl_s32_m_untied:
**	movprfx	z0, z1
**	srshl	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (rshl_s32_m_untied, svint32_t, svint32_t,
	     z0 = svrshl_s32_m (p0, z1, z4),
	     z0 = svrshl_m (p0, z1, z4))

/*
** rshl_w0_s32_m_tied1:
**	mov	(z[0-9]+\.s), w0
**	srshl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_s32_m_tied1, svint32_t, int32_t,
		 z0 = svrshl_n_s32_m (p0, z0, x0),
		 z0 = svrshl_m (p0, z0, x0))

/*
** rshl_w0_s32_m_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	srshl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_s32_m_untied, svint32_t, int32_t,
		 z0 = svrshl_n_s32_m (p0, z1, x0),
		 z0 = svrshl_m (p0, z1, x0))

/*
** rshl_m32_s32_m:
**	srshr	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (rshl_m32_s32_m, svint32_t,
		z0 = svrshl_n_s32_m (p0, z0, -32),
		z0 = svrshl_m (p0, z0, -32))

/*
** rshl_m2_s32_m:
**	srshr	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_m2_s32_m, svint32_t,
		z0 = svrshl_n_s32_m (p0, z0, -2),
		z0 = svrshl_m (p0, z0, -2))

/*
** rshl_m1_s32_m_tied1:
**	srshr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_s32_m_tied1, svint32_t,
		z0 = svrshl_n_s32_m (p0, z0, -1),
		z0 = svrshl_m (p0, z0, -1))

/*
** rshl_m1_s32_m_untied:
**	movprfx	z0, z1
**	srshr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_s32_m_untied, svint32_t,
		z0 = svrshl_n_s32_m (p0, z1, -1),
		z0 = svrshl_m (p0, z1, -1))

/*
** rshl_1_s32_m_tied1:
**	lsl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_s32_m_tied1, svint32_t,
		z0 = svrshl_n_s32_m (p0, z0, 1),
		z0 = svrshl_m (p0, z0, 1))

/*
** rshl_1_s32_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_s32_m_untied, svint32_t,
		z0 = svrshl_n_s32_m (p0, z1, 1),
		z0 = svrshl_m (p0, z1, 1))

/*
** rshl_2_s32_m:
**	lsl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_2_s32_m, svint32_t,
		z0 = svrshl_n_s32_m (p0, z0, 2),
		z0 = svrshl_m (p0, z0, 2))

/*
** rshl_31_s32_m:
**	lsl	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (rshl_31_s32_m, svint32_t,
		z0 = svrshl_n_s32_m (p0, z0, 31),
		z0 = svrshl_m (p0, z0, 31))

/*
** rshl_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	srshl	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (rshl_s32_z_tied1, svint32_t, svint32_t,
	     z0 = svrshl_s32_z (p0, z0, z4),
	     z0 = svrshl_z (p0, z0, z4))

/*
** rshl_s32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	srshlr	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z_REV (rshl_s32_z_tied2, svint32_t, svint32_t,
		 z0_res = svrshl_s32_z (p0, z4, z0),
		 z0_res = svrshl_z (p0, z4, z0))

/*
** rshl_s32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	srshl	z0\.s, p0/m, z0\.s, z4\.s
** |
**	movprfx	z0\.s, p0/z, z4\.s
**	srshlr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_DUAL_Z (rshl_s32_z_untied, svint32_t, svint32_t,
	     z0 = svrshl_s32_z (p0, z1, z4),
	     z0 = svrshl_z (p0, z1, z4))

/*
** rshl_w0_s32_z_tied1:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	srshl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_s32_z_tied1, svint32_t, int32_t,
		 z0 = svrshl_n_s32_z (p0, z0, x0),
		 z0 = svrshl_z (p0, z0, x0))

/*
** rshl_w0_s32_z_untied:
**	mov	(z[0-9]+\.s), w0
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	srshl	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	srshlr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_s32_z_untied, svint32_t, int32_t,
		 z0 = svrshl_n_s32_z (p0, z1, x0),
		 z0 = svrshl_z (p0, z1, x0))

/*
** rshl_m32_s32_z:
**	movprfx	z0\.s, p0/z, z0\.s
**	srshr	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (rshl_m32_s32_z, svint32_t,
		z0 = svrshl_n_s32_z (p0, z0, -32),
		z0 = svrshl_z (p0, z0, -32))

/*
** rshl_m2_s32_z:
**	movprfx	z0\.s, p0/z, z0\.s
**	srshr	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_m2_s32_z, svint32_t,
		z0 = svrshl_n_s32_z (p0, z0, -2),
		z0 = svrshl_z (p0, z0, -2))

/*
** rshl_m1_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	srshr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_s32_z_tied1, svint32_t,
		z0 = svrshl_n_s32_z (p0, z0, -1),
		z0 = svrshl_z (p0, z0, -1))

/*
** rshl_m1_s32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	srshr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_s32_z_untied, svint32_t,
		z0 = svrshl_n_s32_z (p0, z1, -1),
		z0 = svrshl_z (p0, z1, -1))

/*
** rshl_1_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_s32_z_tied1, svint32_t,
		z0 = svrshl_n_s32_z (p0, z0, 1),
		z0 = svrshl_z (p0, z0, 1))

/*
** rshl_1_s32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	lsl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_s32_z_untied, svint32_t,
		z0 = svrshl_n_s32_z (p0, z1, 1),
		z0 = svrshl_z (p0, z1, 1))

/*
** rshl_2_s32_z:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_2_s32_z, svint32_t,
		z0 = svrshl_n_s32_z (p0, z0, 2),
		z0 = svrshl_z (p0, z0, 2))

/*
** rshl_31_s32_z:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsl	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (rshl_31_s32_z, svint32_t,
		z0 = svrshl_n_s32_z (p0, z0, 31),
		z0 = svrshl_z (p0, z0, 31))

/*
** rshl_s32_x_tied1:
**	srshl	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (rshl_s32_x_tied1, svint32_t, svint32_t,
	     z0 = svrshl_s32_x (p0, z0, z4),
	     z0 = svrshl_x (p0, z0, z4))

/*
** rshl_s32_x_tied2:
**	srshlr	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z_REV (rshl_s32_x_tied2, svint32_t, svint32_t,
		 z0_res = svrshl_s32_x (p0, z4, z0),
		 z0_res = svrshl_x (p0, z4, z0))

/*
** rshl_s32_x_untied:
** (
**	movprfx	z0, z1
**	srshl	z0\.s, p0/m, z0\.s, z4\.s
** |
**	movprfx	z0, z4
**	srshlr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_DUAL_Z (rshl_s32_x_untied, svint32_t, svint32_t,
	     z0 = svrshl_s32_x (p0, z1, z4),
	     z0 = svrshl_x (p0, z1, z4))

/*
** rshl_w0_s32_x_tied1:
**	mov	(z[0-9]+\.s), w0
**	srshl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_s32_x_tied1, svint32_t, int32_t,
		 z0 = svrshl_n_s32_x (p0, z0, x0),
		 z0 = svrshl_x (p0, z0, x0))

/*
** rshl_w0_s32_x_untied:
**	mov	z0\.s, w0
**	srshlr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_s32_x_untied, svint32_t, int32_t,
		 z0 = svrshl_n_s32_x (p0, z1, x0),
		 z0 = svrshl_x (p0, z1, x0))

/*
** rshl_m32_s32_x:
**	srshr	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (rshl_m32_s32_x, svint32_t,
		z0 = svrshl_n_s32_x (p0, z0, -32),
		z0 = svrshl_x (p0, z0, -32))

/*
** rshl_m2_s32_x:
**	srshr	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_m2_s32_x, svint32_t,
		z0 = svrshl_n_s32_x (p0, z0, -2),
		z0 = svrshl_x (p0, z0, -2))

/*
** rshl_m1_s32_x_tied1:
**	srshr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_s32_x_tied1, svint32_t,
		z0 = svrshl_n_s32_x (p0, z0, -1),
		z0 = svrshl_x (p0, z0, -1))

/*
** rshl_m1_s32_x_untied:
**	movprfx	z0, z1
**	srshr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_s32_x_untied, svint32_t,
		z0 = svrshl_n_s32_x (p0, z1, -1),
		z0 = svrshl_x (p0, z1, -1))

/*
** rshl_1_s32_x_tied1:
**	lsl	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_s32_x_tied1, svint32_t,
		z0 = svrshl_n_s32_x (p0, z0, 1),
		z0 = svrshl_x (p0, z0, 1))

/*
** rshl_1_s32_x_untied:
**	lsl	z0\.s, z1\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_s32_x_untied, svint32_t,
		z0 = svrshl_n_s32_x (p0, z1, 1),
		z0 = svrshl_x (p0, z1, 1))

/*
** rshl_2_s32_x:
**	lsl	z0\.s, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_2_s32_x, svint32_t,
		z0 = svrshl_n_s32_x (p0, z0, 2),
		z0 = svrshl_x (p0, z0, 2))

/*
** rshl_31_s32_x:
**	lsl	z0\.s, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (rshl_31_s32_x, svint32_t,
		z0 = svrshl_n_s32_x (p0, z0, 31),
		z0 = svrshl_x (p0, z0, 31))
