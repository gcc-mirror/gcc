/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rshl_s16_m_tied1:
**	srshl	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (rshl_s16_m_tied1, svint16_t, svint16_t,
	     z0 = svrshl_s16_m (p0, z0, z4),
	     z0 = svrshl_m (p0, z0, z4))

/*
** rshl_s16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	srshl	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (rshl_s16_m_tied2, svint16_t, svint16_t,
		 z0_res = svrshl_s16_m (p0, z4, z0),
		 z0_res = svrshl_m (p0, z4, z0))

/*
** rshl_s16_m_untied:
**	movprfx	z0, z1
**	srshl	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (rshl_s16_m_untied, svint16_t, svint16_t,
	     z0 = svrshl_s16_m (p0, z1, z4),
	     z0 = svrshl_m (p0, z1, z4))

/*
** rshl_w0_s16_m_tied1:
**	mov	(z[0-9]+\.h), w0
**	srshl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_s16_m_tied1, svint16_t, int16_t,
		 z0 = svrshl_n_s16_m (p0, z0, x0),
		 z0 = svrshl_m (p0, z0, x0))

/*
** rshl_w0_s16_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	srshl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_s16_m_untied, svint16_t, int16_t,
		 z0 = svrshl_n_s16_m (p0, z1, x0),
		 z0 = svrshl_m (p0, z1, x0))

/*
** rshl_m16_s16_m:
**	srshr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (rshl_m16_s16_m, svint16_t,
		z0 = svrshl_n_s16_m (p0, z0, -16),
		z0 = svrshl_m (p0, z0, -16))

/*
** rshl_m2_s16_m:
**	srshr	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_m2_s16_m, svint16_t,
		z0 = svrshl_n_s16_m (p0, z0, -2),
		z0 = svrshl_m (p0, z0, -2))

/*
** rshl_m1_s16_m_tied1:
**	srshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_s16_m_tied1, svint16_t,
		z0 = svrshl_n_s16_m (p0, z0, -1),
		z0 = svrshl_m (p0, z0, -1))

/*
** rshl_m1_s16_m_untied:
**	movprfx	z0, z1
**	srshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_s16_m_untied, svint16_t,
		z0 = svrshl_n_s16_m (p0, z1, -1),
		z0 = svrshl_m (p0, z1, -1))

/*
** rshl_1_s16_m_tied1:
**	lsl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_s16_m_tied1, svint16_t,
		z0 = svrshl_n_s16_m (p0, z0, 1),
		z0 = svrshl_m (p0, z0, 1))

/*
** rshl_1_s16_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_s16_m_untied, svint16_t,
		z0 = svrshl_n_s16_m (p0, z1, 1),
		z0 = svrshl_m (p0, z1, 1))

/*
** rshl_2_s16_m:
**	lsl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_2_s16_m, svint16_t,
		z0 = svrshl_n_s16_m (p0, z0, 2),
		z0 = svrshl_m (p0, z0, 2))

/*
** rshl_15_s16_m:
**	lsl	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (rshl_15_s16_m, svint16_t,
		z0 = svrshl_n_s16_m (p0, z0, 15),
		z0 = svrshl_m (p0, z0, 15))

/*
** rshl_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	srshl	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (rshl_s16_z_tied1, svint16_t, svint16_t,
	     z0 = svrshl_s16_z (p0, z0, z4),
	     z0 = svrshl_z (p0, z0, z4))

/*
** rshl_s16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	srshlr	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z_REV (rshl_s16_z_tied2, svint16_t, svint16_t,
		 z0_res = svrshl_s16_z (p0, z4, z0),
		 z0_res = svrshl_z (p0, z4, z0))

/*
** rshl_s16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	srshl	z0\.h, p0/m, z0\.h, z4\.h
** |
**	movprfx	z0\.h, p0/z, z4\.h
**	srshlr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_DUAL_Z (rshl_s16_z_untied, svint16_t, svint16_t,
	     z0 = svrshl_s16_z (p0, z1, z4),
	     z0 = svrshl_z (p0, z1, z4))

/*
** rshl_w0_s16_z_tied1:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z0\.h
**	srshl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_s16_z_tied1, svint16_t, int16_t,
		 z0 = svrshl_n_s16_z (p0, z0, x0),
		 z0 = svrshl_z (p0, z0, x0))

/*
** rshl_w0_s16_z_untied:
**	mov	(z[0-9]+\.h), w0
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	srshl	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	srshlr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_s16_z_untied, svint16_t, int16_t,
		 z0 = svrshl_n_s16_z (p0, z1, x0),
		 z0 = svrshl_z (p0, z1, x0))

/*
** rshl_m16_s16_z:
**	movprfx	z0\.h, p0/z, z0\.h
**	srshr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (rshl_m16_s16_z, svint16_t,
		z0 = svrshl_n_s16_z (p0, z0, -16),
		z0 = svrshl_z (p0, z0, -16))

/*
** rshl_m2_s16_z:
**	movprfx	z0\.h, p0/z, z0\.h
**	srshr	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_m2_s16_z, svint16_t,
		z0 = svrshl_n_s16_z (p0, z0, -2),
		z0 = svrshl_z (p0, z0, -2))

/*
** rshl_m1_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	srshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_s16_z_tied1, svint16_t,
		z0 = svrshl_n_s16_z (p0, z0, -1),
		z0 = svrshl_z (p0, z0, -1))

/*
** rshl_m1_s16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	srshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_s16_z_untied, svint16_t,
		z0 = svrshl_n_s16_z (p0, z1, -1),
		z0 = svrshl_z (p0, z1, -1))

/*
** rshl_1_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_s16_z_tied1, svint16_t,
		z0 = svrshl_n_s16_z (p0, z0, 1),
		z0 = svrshl_z (p0, z0, 1))

/*
** rshl_1_s16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	lsl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_s16_z_untied, svint16_t,
		z0 = svrshl_n_s16_z (p0, z1, 1),
		z0 = svrshl_z (p0, z1, 1))

/*
** rshl_2_s16_z:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_2_s16_z, svint16_t,
		z0 = svrshl_n_s16_z (p0, z0, 2),
		z0 = svrshl_z (p0, z0, 2))

/*
** rshl_15_s16_z:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsl	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (rshl_15_s16_z, svint16_t,
		z0 = svrshl_n_s16_z (p0, z0, 15),
		z0 = svrshl_z (p0, z0, 15))

/*
** rshl_s16_x_tied1:
**	srshl	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (rshl_s16_x_tied1, svint16_t, svint16_t,
	     z0 = svrshl_s16_x (p0, z0, z4),
	     z0 = svrshl_x (p0, z0, z4))

/*
** rshl_s16_x_tied2:
**	srshlr	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z_REV (rshl_s16_x_tied2, svint16_t, svint16_t,
		 z0_res = svrshl_s16_x (p0, z4, z0),
		 z0_res = svrshl_x (p0, z4, z0))

/*
** rshl_s16_x_untied:
** (
**	movprfx	z0, z1
**	srshl	z0\.h, p0/m, z0\.h, z4\.h
** |
**	movprfx	z0, z4
**	srshlr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_DUAL_Z (rshl_s16_x_untied, svint16_t, svint16_t,
	     z0 = svrshl_s16_x (p0, z1, z4),
	     z0 = svrshl_x (p0, z1, z4))

/*
** rshl_w0_s16_x_tied1:
**	mov	(z[0-9]+\.h), w0
**	srshl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_s16_x_tied1, svint16_t, int16_t,
		 z0 = svrshl_n_s16_x (p0, z0, x0),
		 z0 = svrshl_x (p0, z0, x0))

/*
** rshl_w0_s16_x_untied:
**	mov	z0\.h, w0
**	srshlr	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_s16_x_untied, svint16_t, int16_t,
		 z0 = svrshl_n_s16_x (p0, z1, x0),
		 z0 = svrshl_x (p0, z1, x0))

/*
** rshl_m16_s16_x:
**	srshr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (rshl_m16_s16_x, svint16_t,
		z0 = svrshl_n_s16_x (p0, z0, -16),
		z0 = svrshl_x (p0, z0, -16))

/*
** rshl_m2_s16_x:
**	srshr	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_m2_s16_x, svint16_t,
		z0 = svrshl_n_s16_x (p0, z0, -2),
		z0 = svrshl_x (p0, z0, -2))

/*
** rshl_m1_s16_x_tied1:
**	srshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_s16_x_tied1, svint16_t,
		z0 = svrshl_n_s16_x (p0, z0, -1),
		z0 = svrshl_x (p0, z0, -1))

/*
** rshl_m1_s16_x_untied:
**	movprfx	z0, z1
**	srshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_s16_x_untied, svint16_t,
		z0 = svrshl_n_s16_x (p0, z1, -1),
		z0 = svrshl_x (p0, z1, -1))

/*
** rshl_1_s16_x_tied1:
**	lsl	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_s16_x_tied1, svint16_t,
		z0 = svrshl_n_s16_x (p0, z0, 1),
		z0 = svrshl_x (p0, z0, 1))

/*
** rshl_1_s16_x_untied:
**	lsl	z0\.h, z1\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_s16_x_untied, svint16_t,
		z0 = svrshl_n_s16_x (p0, z1, 1),
		z0 = svrshl_x (p0, z1, 1))

/*
** rshl_2_s16_x:
**	lsl	z0\.h, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_2_s16_x, svint16_t,
		z0 = svrshl_n_s16_x (p0, z0, 2),
		z0 = svrshl_x (p0, z0, 2))

/*
** rshl_15_s16_x:
**	lsl	z0\.h, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (rshl_15_s16_x, svint16_t,
		z0 = svrshl_n_s16_x (p0, z0, 15),
		z0 = svrshl_x (p0, z0, 15))
