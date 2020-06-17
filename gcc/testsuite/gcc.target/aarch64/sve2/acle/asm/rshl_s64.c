/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rshl_s64_m_tied1:
**	srshl	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (rshl_s64_m_tied1, svint64_t, svint64_t,
	     z0 = svrshl_s64_m (p0, z0, z4),
	     z0 = svrshl_m (p0, z0, z4))

/*
** rshl_s64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z4
**	srshl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_DUAL_Z_REV (rshl_s64_m_tied2, svint64_t, svint64_t,
		 z0_res = svrshl_s64_m (p0, z4, z0),
		 z0_res = svrshl_m (p0, z4, z0))

/*
** rshl_s64_m_untied:
**	movprfx	z0, z1
**	srshl	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (rshl_s64_m_untied, svint64_t, svint64_t,
	     z0 = svrshl_s64_m (p0, z1, z4),
	     z0 = svrshl_m (p0, z1, z4))

/*
** rshl_x0_s64_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	srshl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (rshl_x0_s64_m_tied1, svint64_t, int64_t,
		 z0 = svrshl_n_s64_m (p0, z0, x0),
		 z0 = svrshl_m (p0, z0, x0))

/*
** rshl_x0_s64_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	srshl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (rshl_x0_s64_m_untied, svint64_t, int64_t,
		 z0 = svrshl_n_s64_m (p0, z1, x0),
		 z0 = svrshl_m (p0, z1, x0))

/*
** rshl_m64_s64_m:
**	srshr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (rshl_m64_s64_m, svint64_t,
		z0 = svrshl_n_s64_m (p0, z0, -64),
		z0 = svrshl_m (p0, z0, -64))

/*
** rshl_m2_s64_m:
**	srshr	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_m2_s64_m, svint64_t,
		z0 = svrshl_n_s64_m (p0, z0, -2),
		z0 = svrshl_m (p0, z0, -2))

/*
** rshl_m1_s64_m_tied1:
**	srshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_s64_m_tied1, svint64_t,
		z0 = svrshl_n_s64_m (p0, z0, -1),
		z0 = svrshl_m (p0, z0, -1))

/*
** rshl_m1_s64_m_untied:
**	movprfx	z0, z1
**	srshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_s64_m_untied, svint64_t,
		z0 = svrshl_n_s64_m (p0, z1, -1),
		z0 = svrshl_m (p0, z1, -1))

/*
** rshl_1_s64_m_tied1:
**	lsl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_s64_m_tied1, svint64_t,
		z0 = svrshl_n_s64_m (p0, z0, 1),
		z0 = svrshl_m (p0, z0, 1))

/*
** rshl_1_s64_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_s64_m_untied, svint64_t,
		z0 = svrshl_n_s64_m (p0, z1, 1),
		z0 = svrshl_m (p0, z1, 1))

/*
** rshl_2_s64_m:
**	lsl	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_2_s64_m, svint64_t,
		z0 = svrshl_n_s64_m (p0, z0, 2),
		z0 = svrshl_m (p0, z0, 2))

/*
** rshl_63_s64_m:
**	lsl	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (rshl_63_s64_m, svint64_t,
		z0 = svrshl_n_s64_m (p0, z0, 63),
		z0 = svrshl_m (p0, z0, 63))

/*
** rshl_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	srshl	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (rshl_s64_z_tied1, svint64_t, svint64_t,
	     z0 = svrshl_s64_z (p0, z0, z4),
	     z0 = svrshl_z (p0, z0, z4))

/*
** rshl_s64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	srshlr	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z_REV (rshl_s64_z_tied2, svint64_t, svint64_t,
		 z0_res = svrshl_s64_z (p0, z4, z0),
		 z0_res = svrshl_z (p0, z4, z0))

/*
** rshl_s64_z_untied:
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	srshl	z0\.d, p0/m, z0\.d, z4\.d
** |
**	movprfx	z0\.d, p0/z, z4\.d
**	srshlr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (rshl_s64_z_untied, svint64_t, svint64_t,
	     z0 = svrshl_s64_z (p0, z1, z4),
	     z0 = svrshl_z (p0, z1, z4))

/*
** rshl_x0_s64_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	srshl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (rshl_x0_s64_z_tied1, svint64_t, int64_t,
		 z0 = svrshl_n_s64_z (p0, z0, x0),
		 z0 = svrshl_z (p0, z0, x0))

/*
** rshl_x0_s64_z_untied:
**	mov	(z[0-9]+\.d), x0
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	srshl	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	srshlr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_ZX (rshl_x0_s64_z_untied, svint64_t, int64_t,
		 z0 = svrshl_n_s64_z (p0, z1, x0),
		 z0 = svrshl_z (p0, z1, x0))

/*
** rshl_m64_s64_z:
**	movprfx	z0\.d, p0/z, z0\.d
**	srshr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (rshl_m64_s64_z, svint64_t,
		z0 = svrshl_n_s64_z (p0, z0, -64),
		z0 = svrshl_z (p0, z0, -64))

/*
** rshl_m2_s64_z:
**	movprfx	z0\.d, p0/z, z0\.d
**	srshr	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_m2_s64_z, svint64_t,
		z0 = svrshl_n_s64_z (p0, z0, -2),
		z0 = svrshl_z (p0, z0, -2))

/*
** rshl_m1_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	srshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_s64_z_tied1, svint64_t,
		z0 = svrshl_n_s64_z (p0, z0, -1),
		z0 = svrshl_z (p0, z0, -1))

/*
** rshl_m1_s64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	srshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_s64_z_untied, svint64_t,
		z0 = svrshl_n_s64_z (p0, z1, -1),
		z0 = svrshl_z (p0, z1, -1))

/*
** rshl_1_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	lsl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_s64_z_tied1, svint64_t,
		z0 = svrshl_n_s64_z (p0, z0, 1),
		z0 = svrshl_z (p0, z0, 1))

/*
** rshl_1_s64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	lsl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_s64_z_untied, svint64_t,
		z0 = svrshl_n_s64_z (p0, z1, 1),
		z0 = svrshl_z (p0, z1, 1))

/*
** rshl_2_s64_z:
**	movprfx	z0\.d, p0/z, z0\.d
**	lsl	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_2_s64_z, svint64_t,
		z0 = svrshl_n_s64_z (p0, z0, 2),
		z0 = svrshl_z (p0, z0, 2))

/*
** rshl_63_s64_z:
**	movprfx	z0\.d, p0/z, z0\.d
**	lsl	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (rshl_63_s64_z, svint64_t,
		z0 = svrshl_n_s64_z (p0, z0, 63),
		z0 = svrshl_z (p0, z0, 63))

/*
** rshl_s64_x_tied1:
**	srshl	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (rshl_s64_x_tied1, svint64_t, svint64_t,
	     z0 = svrshl_s64_x (p0, z0, z4),
	     z0 = svrshl_x (p0, z0, z4))

/*
** rshl_s64_x_tied2:
**	srshlr	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z_REV (rshl_s64_x_tied2, svint64_t, svint64_t,
		 z0_res = svrshl_s64_x (p0, z4, z0),
		 z0_res = svrshl_x (p0, z4, z0))

/*
** rshl_s64_x_untied:
** (
**	movprfx	z0, z1
**	srshl	z0\.d, p0/m, z0\.d, z4\.d
** |
**	movprfx	z0, z4
**	srshlr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (rshl_s64_x_untied, svint64_t, svint64_t,
	     z0 = svrshl_s64_x (p0, z1, z4),
	     z0 = svrshl_x (p0, z1, z4))

/*
** rshl_x0_s64_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	srshl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (rshl_x0_s64_x_tied1, svint64_t, int64_t,
		 z0 = svrshl_n_s64_x (p0, z0, x0),
		 z0 = svrshl_x (p0, z0, x0))

/*
** rshl_x0_s64_x_untied:
**	mov	z0\.d, x0
**	srshlr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_ZX (rshl_x0_s64_x_untied, svint64_t, int64_t,
		 z0 = svrshl_n_s64_x (p0, z1, x0),
		 z0 = svrshl_x (p0, z1, x0))

/*
** rshl_m64_s64_x:
**	srshr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (rshl_m64_s64_x, svint64_t,
		z0 = svrshl_n_s64_x (p0, z0, -64),
		z0 = svrshl_x (p0, z0, -64))

/*
** rshl_m2_s64_x:
**	srshr	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_m2_s64_x, svint64_t,
		z0 = svrshl_n_s64_x (p0, z0, -2),
		z0 = svrshl_x (p0, z0, -2))

/*
** rshl_m1_s64_x_tied1:
**	srshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_s64_x_tied1, svint64_t,
		z0 = svrshl_n_s64_x (p0, z0, -1),
		z0 = svrshl_x (p0, z0, -1))

/*
** rshl_m1_s64_x_untied:
**	movprfx	z0, z1
**	srshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_m1_s64_x_untied, svint64_t,
		z0 = svrshl_n_s64_x (p0, z1, -1),
		z0 = svrshl_x (p0, z1, -1))

/*
** rshl_1_s64_x_tied1:
**	lsl	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_s64_x_tied1, svint64_t,
		z0 = svrshl_n_s64_x (p0, z0, 1),
		z0 = svrshl_x (p0, z0, 1))

/*
** rshl_1_s64_x_untied:
**	lsl	z0\.d, z1\.d, #1
**	ret
*/
TEST_UNIFORM_Z (rshl_1_s64_x_untied, svint64_t,
		z0 = svrshl_n_s64_x (p0, z1, 1),
		z0 = svrshl_x (p0, z1, 1))

/*
** rshl_2_s64_x:
**	lsl	z0\.d, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (rshl_2_s64_x, svint64_t,
		z0 = svrshl_n_s64_x (p0, z0, 2),
		z0 = svrshl_x (p0, z0, 2))

/*
** rshl_63_s64_x:
**	lsl	z0\.d, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (rshl_63_s64_x, svint64_t,
		z0 = svrshl_n_s64_x (p0, z0, 63),
		z0 = svrshl_x (p0, z0, 63))
