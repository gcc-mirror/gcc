/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rshl_s8_m_tied1:
**	srshl	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (rshl_s8_m_tied1, svint8_t, svint8_t,
	     z0 = svrshl_s8_m (p0, z0, z4),
	     z0 = svrshl_m (p0, z0, z4))

/*
** rshl_s8_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	srshl	z0\.b, p0/m, z0\.b, \1\.b
**	ret
*/
TEST_DUAL_Z_REV (rshl_s8_m_tied2, svint8_t, svint8_t,
		 z0_res = svrshl_s8_m (p0, z4, z0),
		 z0_res = svrshl_m (p0, z4, z0))

/*
** rshl_s8_m_untied:
**	movprfx	z0, z1
**	srshl	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (rshl_s8_m_untied, svint8_t, svint8_t,
	     z0 = svrshl_s8_m (p0, z1, z4),
	     z0 = svrshl_m (p0, z1, z4))

/*
** rshl_w0_s8_m_tied1:
**	mov	(z[0-9]+\.b), w0
**	srshl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_s8_m_tied1, svint8_t, int8_t,
		 z0 = svrshl_n_s8_m (p0, z0, x0),
		 z0 = svrshl_m (p0, z0, x0))

/*
** rshl_w0_s8_m_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	srshl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_s8_m_untied, svint8_t, int8_t,
		 z0 = svrshl_n_s8_m (p0, z1, x0),
		 z0 = svrshl_m (p0, z1, x0))

/*
** rshl_m8_s8_m:
**	srshr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (rshl_m8_s8_m, svint8_t,
		z0 = svrshl_n_s8_m (p0, z0, -8),
		z0 = svrshl_m (p0, z0, -8))

/*
** rshl_m2_s8_m:
**	srshr	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_m2_s8_m, svint8_t,
		z0 = svrshl_n_s8_m (p0, z0, -2),
		z0 = svrshl_m (p0, z0, -2))

/*
** rshl_m1_s8_m_tied1:
**	srshr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_s8_m_tied1, svint8_t,
		z0 = svrshl_n_s8_m (p0, z0, -1),
		z0 = svrshl_m (p0, z0, -1))

/*
** rshl_m1_s8_m_untied:
**	movprfx	z0, z1
**	srshr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_s8_m_untied, svint8_t,
		z0 = svrshl_n_s8_m (p0, z1, -1),
		z0 = svrshl_m (p0, z1, -1))

/*
** rshl_1_s8_m_tied1:
**	lsl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_s8_m_tied1, svint8_t,
		z0 = svrshl_n_s8_m (p0, z0, 1),
		z0 = svrshl_m (p0, z0, 1))

/*
** rshl_1_s8_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_s8_m_untied, svint8_t,
		z0 = svrshl_n_s8_m (p0, z1, 1),
		z0 = svrshl_m (p0, z1, 1))

/*
** rshl_2_s8_m:
**	lsl	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_2_s8_m, svint8_t,
		z0 = svrshl_n_s8_m (p0, z0, 2),
		z0 = svrshl_m (p0, z0, 2))

/*
** rshl_7_s8_m:
**	lsl	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (rshl_7_s8_m, svint8_t,
		z0 = svrshl_n_s8_m (p0, z0, 7),
		z0 = svrshl_m (p0, z0, 7))

/*
** rshl_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	srshl	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (rshl_s8_z_tied1, svint8_t, svint8_t,
	     z0 = svrshl_s8_z (p0, z0, z4),
	     z0 = svrshl_z (p0, z0, z4))

/*
** rshl_s8_z_tied2:
**	movprfx	z0\.b, p0/z, z0\.b
**	srshlr	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z_REV (rshl_s8_z_tied2, svint8_t, svint8_t,
		 z0_res = svrshl_s8_z (p0, z4, z0),
		 z0_res = svrshl_z (p0, z4, z0))

/*
** rshl_s8_z_untied:
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	srshl	z0\.b, p0/m, z0\.b, z4\.b
** |
**	movprfx	z0\.b, p0/z, z4\.b
**	srshlr	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_DUAL_Z (rshl_s8_z_untied, svint8_t, svint8_t,
	     z0 = svrshl_s8_z (p0, z1, z4),
	     z0 = svrshl_z (p0, z1, z4))

/*
** rshl_w0_s8_z_tied1:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0\.b, p0/z, z0\.b
**	srshl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_s8_z_tied1, svint8_t, int8_t,
		 z0 = svrshl_n_s8_z (p0, z0, x0),
		 z0 = svrshl_z (p0, z0, x0))

/*
** rshl_w0_s8_z_untied:
**	mov	(z[0-9]+\.b), w0
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	srshl	z0\.b, p0/m, z0\.b, \1
** |
**	movprfx	z0\.b, p0/z, \1
**	srshlr	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_s8_z_untied, svint8_t, int8_t,
		 z0 = svrshl_n_s8_z (p0, z1, x0),
		 z0 = svrshl_z (p0, z1, x0))

/*
** rshl_m8_s8_z:
**	movprfx	z0\.b, p0/z, z0\.b
**	srshr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (rshl_m8_s8_z, svint8_t,
		z0 = svrshl_n_s8_z (p0, z0, -8),
		z0 = svrshl_z (p0, z0, -8))

/*
** rshl_m2_s8_z:
**	movprfx	z0\.b, p0/z, z0\.b
**	srshr	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_m2_s8_z, svint8_t,
		z0 = svrshl_n_s8_z (p0, z0, -2),
		z0 = svrshl_z (p0, z0, -2))

/*
** rshl_m1_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	srshr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_s8_z_tied1, svint8_t,
		z0 = svrshl_n_s8_z (p0, z0, -1),
		z0 = svrshl_z (p0, z0, -1))

/*
** rshl_m1_s8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	srshr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_s8_z_untied, svint8_t,
		z0 = svrshl_n_s8_z (p0, z1, -1),
		z0 = svrshl_z (p0, z1, -1))

/*
** rshl_1_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	lsl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_s8_z_tied1, svint8_t,
		z0 = svrshl_n_s8_z (p0, z0, 1),
		z0 = svrshl_z (p0, z0, 1))

/*
** rshl_1_s8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	lsl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_s8_z_untied, svint8_t,
		z0 = svrshl_n_s8_z (p0, z1, 1),
		z0 = svrshl_z (p0, z1, 1))

/*
** rshl_2_s8_z:
**	movprfx	z0\.b, p0/z, z0\.b
**	lsl	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_2_s8_z, svint8_t,
		z0 = svrshl_n_s8_z (p0, z0, 2),
		z0 = svrshl_z (p0, z0, 2))

/*
** rshl_7_s8_z:
**	movprfx	z0\.b, p0/z, z0\.b
**	lsl	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (rshl_7_s8_z, svint8_t,
		z0 = svrshl_n_s8_z (p0, z0, 7),
		z0 = svrshl_z (p0, z0, 7))

/*
** rshl_s8_x_tied1:
**	srshl	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (rshl_s8_x_tied1, svint8_t, svint8_t,
	     z0 = svrshl_s8_x (p0, z0, z4),
	     z0 = svrshl_x (p0, z0, z4))

/*
** rshl_s8_x_tied2:
**	srshlr	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z_REV (rshl_s8_x_tied2, svint8_t, svint8_t,
		 z0_res = svrshl_s8_x (p0, z4, z0),
		 z0_res = svrshl_x (p0, z4, z0))

/*
** rshl_s8_x_untied:
** (
**	movprfx	z0, z1
**	srshl	z0\.b, p0/m, z0\.b, z4\.b
** |
**	movprfx	z0, z4
**	srshlr	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_DUAL_Z (rshl_s8_x_untied, svint8_t, svint8_t,
	     z0 = svrshl_s8_x (p0, z1, z4),
	     z0 = svrshl_x (p0, z1, z4))

/*
** rshl_w0_s8_x_tied1:
**	mov	(z[0-9]+\.b), w0
**	srshl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_s8_x_tied1, svint8_t, int8_t,
		 z0 = svrshl_n_s8_x (p0, z0, x0),
		 z0 = svrshl_x (p0, z0, x0))

/*
** rshl_w0_s8_x_untied:
**	mov	z0\.b, w0
**	srshlr	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_ZX (rshl_w0_s8_x_untied, svint8_t, int8_t,
		 z0 = svrshl_n_s8_x (p0, z1, x0),
		 z0 = svrshl_x (p0, z1, x0))

/*
** rshl_m8_s8_x:
**	srshr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (rshl_m8_s8_x, svint8_t,
		z0 = svrshl_n_s8_x (p0, z0, -8),
		z0 = svrshl_x (p0, z0, -8))

/*
** rshl_m2_s8_x:
**	srshr	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_m2_s8_x, svint8_t,
		z0 = svrshl_n_s8_x (p0, z0, -2),
		z0 = svrshl_x (p0, z0, -2))

/*
** rshl_m1_s8_x_tied1:
**	srshr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_s8_x_tied1, svint8_t,
		z0 = svrshl_n_s8_x (p0, z0, -1),
		z0 = svrshl_x (p0, z0, -1))

/*
** rshl_m1_s8_x_untied:
**	movprfx	z0, z1
**	srshr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_s8_x_untied, svint8_t,
		z0 = svrshl_n_s8_x (p0, z1, -1),
		z0 = svrshl_x (p0, z1, -1))

/*
** rshl_1_s8_x_tied1:
**	lsl	z0\.b, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_s8_x_tied1, svint8_t,
		z0 = svrshl_n_s8_x (p0, z0, 1),
		z0 = svrshl_x (p0, z0, 1))

/*
** rshl_1_s8_x_untied:
**	lsl	z0\.b, z1\.b, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_s8_x_untied, svint8_t,
		z0 = svrshl_n_s8_x (p0, z1, 1),
		z0 = svrshl_x (p0, z1, 1))

/*
** rshl_2_s8_x:
**	lsl	z0\.b, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_2_s8_x, svint8_t,
		z0 = svrshl_n_s8_x (p0, z0, 2),
		z0 = svrshl_x (p0, z0, 2))

/*
** rshl_7_s8_x:
**	lsl	z0\.b, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (rshl_7_s8_x, svint8_t,
		z0 = svrshl_n_s8_x (p0, z0, 7),
		z0 = svrshl_x (p0, z0, 7))
