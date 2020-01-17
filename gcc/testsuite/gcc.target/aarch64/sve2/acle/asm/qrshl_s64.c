/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrshl_s64_m_tied1:
**	sqrshl	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (qrshl_s64_m_tied1, svint64_t, svint64_t,
	     z0 = svqrshl_s64_m (p0, z0, z4),
	     z0 = svqrshl_m (p0, z0, z4))

/*
** qrshl_s64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z4
**	sqrshl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_DUAL_Z_REV (qrshl_s64_m_tied2, svint64_t, svint64_t,
		 z0_res = svqrshl_s64_m (p0, z4, z0),
		 z0_res = svqrshl_m (p0, z4, z0))

/*
** qrshl_s64_m_untied:
**	movprfx	z0, z1
**	sqrshl	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (qrshl_s64_m_untied, svint64_t, svint64_t,
	     z0 = svqrshl_s64_m (p0, z1, z4),
	     z0 = svqrshl_m (p0, z1, z4))

/*
** qrshl_x0_s64_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	sqrshl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_x0_s64_m_tied1, svint64_t, int64_t,
		 z0 = svqrshl_n_s64_m (p0, z0, x0),
		 z0 = svqrshl_m (p0, z0, x0))

/*
** qrshl_x0_s64_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	sqrshl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_x0_s64_m_untied, svint64_t, int64_t,
		 z0 = svqrshl_n_s64_m (p0, z1, x0),
		 z0 = svqrshl_m (p0, z1, x0))

/*
** qrshl_m64_s64_m:
**	srshr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (qrshl_m64_s64_m, svint64_t,
		z0 = svqrshl_n_s64_m (p0, z0, -64),
		z0 = svqrshl_m (p0, z0, -64))

/*
** qrshl_m2_s64_m:
**	srshr	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_m2_s64_m, svint64_t,
		z0 = svqrshl_n_s64_m (p0, z0, -2),
		z0 = svqrshl_m (p0, z0, -2))

/*
** qrshl_m1_s64_m_tied1:
**	srshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_s64_m_tied1, svint64_t,
		z0 = svqrshl_n_s64_m (p0, z0, -1),
		z0 = svqrshl_m (p0, z0, -1))

/*
** qrshl_m1_s64_m_untied:
**	movprfx	z0, z1
**	srshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_s64_m_untied, svint64_t,
		z0 = svqrshl_n_s64_m (p0, z1, -1),
		z0 = svqrshl_m (p0, z1, -1))

/*
** qrshl_1_s64_m_tied1:
**	sqshl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_s64_m_tied1, svint64_t,
		z0 = svqrshl_n_s64_m (p0, z0, 1),
		z0 = svqrshl_m (p0, z0, 1))

/*
** qrshl_1_s64_m_untied:
**	movprfx	z0, z1
**	sqshl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_s64_m_untied, svint64_t,
		z0 = svqrshl_n_s64_m (p0, z1, 1),
		z0 = svqrshl_m (p0, z1, 1))

/*
** qrshl_2_s64_m:
**	sqshl	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_2_s64_m, svint64_t,
		z0 = svqrshl_n_s64_m (p0, z0, 2),
		z0 = svqrshl_m (p0, z0, 2))

/*
** qrshl_63_s64_m:
**	sqshl	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (qrshl_63_s64_m, svint64_t,
		z0 = svqrshl_n_s64_m (p0, z0, 63),
		z0 = svqrshl_m (p0, z0, 63))

/*
** qrshl_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	sqrshl	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (qrshl_s64_z_tied1, svint64_t, svint64_t,
	     z0 = svqrshl_s64_z (p0, z0, z4),
	     z0 = svqrshl_z (p0, z0, z4))

/*
** qrshl_s64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	sqrshlr	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z_REV (qrshl_s64_z_tied2, svint64_t, svint64_t,
		 z0_res = svqrshl_s64_z (p0, z4, z0),
		 z0_res = svqrshl_z (p0, z4, z0))

/*
** qrshl_s64_z_untied:
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	sqrshl	z0\.d, p0/m, z0\.d, z4\.d
** |
**	movprfx	z0\.d, p0/z, z4\.d
**	sqrshlr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qrshl_s64_z_untied, svint64_t, svint64_t,
	     z0 = svqrshl_s64_z (p0, z1, z4),
	     z0 = svqrshl_z (p0, z1, z4))

/*
** qrshl_x0_s64_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	sqrshl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_x0_s64_z_tied1, svint64_t, int64_t,
		 z0 = svqrshl_n_s64_z (p0, z0, x0),
		 z0 = svqrshl_z (p0, z0, x0))

/*
** qrshl_x0_s64_z_untied:
**	mov	(z[0-9]+\.d), x0
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	sqrshl	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	sqrshlr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_ZX (qrshl_x0_s64_z_untied, svint64_t, int64_t,
		 z0 = svqrshl_n_s64_z (p0, z1, x0),
		 z0 = svqrshl_z (p0, z1, x0))

/*
** qrshl_m64_s64_z:
**	movprfx	z0\.d, p0/z, z0\.d
**	srshr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (qrshl_m64_s64_z, svint64_t,
		z0 = svqrshl_n_s64_z (p0, z0, -64),
		z0 = svqrshl_z (p0, z0, -64))

/*
** qrshl_m2_s64_z:
**	movprfx	z0\.d, p0/z, z0\.d
**	srshr	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_m2_s64_z, svint64_t,
		z0 = svqrshl_n_s64_z (p0, z0, -2),
		z0 = svqrshl_z (p0, z0, -2))

/*
** qrshl_m1_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	srshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_s64_z_tied1, svint64_t,
		z0 = svqrshl_n_s64_z (p0, z0, -1),
		z0 = svqrshl_z (p0, z0, -1))

/*
** qrshl_m1_s64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	srshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_s64_z_untied, svint64_t,
		z0 = svqrshl_n_s64_z (p0, z1, -1),
		z0 = svqrshl_z (p0, z1, -1))

/*
** qrshl_1_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	sqshl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_s64_z_tied1, svint64_t,
		z0 = svqrshl_n_s64_z (p0, z0, 1),
		z0 = svqrshl_z (p0, z0, 1))

/*
** qrshl_1_s64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	sqshl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_s64_z_untied, svint64_t,
		z0 = svqrshl_n_s64_z (p0, z1, 1),
		z0 = svqrshl_z (p0, z1, 1))

/*
** qrshl_2_s64_z:
**	movprfx	z0\.d, p0/z, z0\.d
**	sqshl	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_2_s64_z, svint64_t,
		z0 = svqrshl_n_s64_z (p0, z0, 2),
		z0 = svqrshl_z (p0, z0, 2))

/*
** qrshl_63_s64_z:
**	movprfx	z0\.d, p0/z, z0\.d
**	sqshl	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (qrshl_63_s64_z, svint64_t,
		z0 = svqrshl_n_s64_z (p0, z0, 63),
		z0 = svqrshl_z (p0, z0, 63))

/*
** qrshl_s64_x_tied1:
**	sqrshl	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (qrshl_s64_x_tied1, svint64_t, svint64_t,
	     z0 = svqrshl_s64_x (p0, z0, z4),
	     z0 = svqrshl_x (p0, z0, z4))

/*
** qrshl_s64_x_tied2:
**	sqrshlr	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z_REV (qrshl_s64_x_tied2, svint64_t, svint64_t,
		 z0_res = svqrshl_s64_x (p0, z4, z0),
		 z0_res = svqrshl_x (p0, z4, z0))

/*
** qrshl_s64_x_untied:
** (
**	movprfx	z0, z1
**	sqrshl	z0\.d, p0/m, z0\.d, z4\.d
** |
**	movprfx	z0, z4
**	sqrshlr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qrshl_s64_x_untied, svint64_t, svint64_t,
	     z0 = svqrshl_s64_x (p0, z1, z4),
	     z0 = svqrshl_x (p0, z1, z4))

/*
** qrshl_x0_s64_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	sqrshl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_x0_s64_x_tied1, svint64_t, int64_t,
		 z0 = svqrshl_n_s64_x (p0, z0, x0),
		 z0 = svqrshl_x (p0, z0, x0))

/*
** qrshl_x0_s64_x_untied:
**	mov	z0\.d, x0
**	sqrshlr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_ZX (qrshl_x0_s64_x_untied, svint64_t, int64_t,
		 z0 = svqrshl_n_s64_x (p0, z1, x0),
		 z0 = svqrshl_x (p0, z1, x0))

/*
** qrshl_m64_s64_x:
**	srshr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (qrshl_m64_s64_x, svint64_t,
		z0 = svqrshl_n_s64_x (p0, z0, -64),
		z0 = svqrshl_x (p0, z0, -64))

/*
** qrshl_m2_s64_x:
**	srshr	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_m2_s64_x, svint64_t,
		z0 = svqrshl_n_s64_x (p0, z0, -2),
		z0 = svqrshl_x (p0, z0, -2))

/*
** qrshl_m1_s64_x_tied1:
**	srshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_s64_x_tied1, svint64_t,
		z0 = svqrshl_n_s64_x (p0, z0, -1),
		z0 = svqrshl_x (p0, z0, -1))

/*
** qrshl_m1_s64_x_untied:
**	movprfx	z0, z1
**	srshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_s64_x_untied, svint64_t,
		z0 = svqrshl_n_s64_x (p0, z1, -1),
		z0 = svqrshl_x (p0, z1, -1))

/*
** qrshl_1_s64_x_tied1:
**	sqshl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_s64_x_tied1, svint64_t,
		z0 = svqrshl_n_s64_x (p0, z0, 1),
		z0 = svqrshl_x (p0, z0, 1))

/*
** qrshl_1_s64_x_untied:
**	movprfx	z0, z1
**	sqshl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_s64_x_untied, svint64_t,
		z0 = svqrshl_n_s64_x (p0, z1, 1),
		z0 = svqrshl_x (p0, z1, 1))

/*
** qrshl_2_s64_x:
**	sqshl	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_2_s64_x, svint64_t,
		z0 = svqrshl_n_s64_x (p0, z0, 2),
		z0 = svqrshl_x (p0, z0, 2))

/*
** qrshl_63_s64_x:
**	sqshl	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (qrshl_63_s64_x, svint64_t,
		z0 = svqrshl_n_s64_x (p0, z0, 63),
		z0 = svqrshl_x (p0, z0, 63))
