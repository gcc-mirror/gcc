/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrshl_u64_m_tied1:
**	uqrshl	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (qrshl_u64_m_tied1, svuint64_t, svint64_t,
	     z0 = svqrshl_u64_m (p0, z0, z4),
	     z0 = svqrshl_m (p0, z0, z4))

/*
** qrshl_u64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z4
**	uqrshl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_DUAL_Z_REV (qrshl_u64_m_tied2, svuint64_t, svint64_t,
		 z0_res = svqrshl_u64_m (p0, z4, z0),
		 z0_res = svqrshl_m (p0, z4, z0))

/*
** qrshl_u64_m_untied:
**	movprfx	z0, z1
**	uqrshl	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (qrshl_u64_m_untied, svuint64_t, svint64_t,
	     z0 = svqrshl_u64_m (p0, z1, z4),
	     z0 = svqrshl_m (p0, z1, z4))

/*
** qrshl_x0_u64_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	uqrshl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_x0_u64_m_tied1, svuint64_t, int64_t,
		 z0 = svqrshl_n_u64_m (p0, z0, x0),
		 z0 = svqrshl_m (p0, z0, x0))

/*
** qrshl_x0_u64_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	uqrshl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_x0_u64_m_untied, svuint64_t, int64_t,
		 z0 = svqrshl_n_u64_m (p0, z1, x0),
		 z0 = svqrshl_m (p0, z1, x0))

/*
** qrshl_m64_u64_m:
**	urshr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (qrshl_m64_u64_m, svuint64_t,
		z0 = svqrshl_n_u64_m (p0, z0, -64),
		z0 = svqrshl_m (p0, z0, -64))

/*
** qrshl_m2_u64_m:
**	urshr	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_m2_u64_m, svuint64_t,
		z0 = svqrshl_n_u64_m (p0, z0, -2),
		z0 = svqrshl_m (p0, z0, -2))

/*
** qrshl_m1_u64_m_tied1:
**	urshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_u64_m_tied1, svuint64_t,
		z0 = svqrshl_n_u64_m (p0, z0, -1),
		z0 = svqrshl_m (p0, z0, -1))

/*
** qrshl_m1_u64_m_untied:
**	movprfx	z0, z1
**	urshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_u64_m_untied, svuint64_t,
		z0 = svqrshl_n_u64_m (p0, z1, -1),
		z0 = svqrshl_m (p0, z1, -1))

/*
** qrshl_1_u64_m_tied1:
**	uqshl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_u64_m_tied1, svuint64_t,
		z0 = svqrshl_n_u64_m (p0, z0, 1),
		z0 = svqrshl_m (p0, z0, 1))

/*
** qrshl_1_u64_m_untied:
**	movprfx	z0, z1
**	uqshl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_u64_m_untied, svuint64_t,
		z0 = svqrshl_n_u64_m (p0, z1, 1),
		z0 = svqrshl_m (p0, z1, 1))

/*
** qrshl_2_u64_m:
**	uqshl	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_2_u64_m, svuint64_t,
		z0 = svqrshl_n_u64_m (p0, z0, 2),
		z0 = svqrshl_m (p0, z0, 2))

/*
** qrshl_63_u64_m:
**	uqshl	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (qrshl_63_u64_m, svuint64_t,
		z0 = svqrshl_n_u64_m (p0, z0, 63),
		z0 = svqrshl_m (p0, z0, 63))

/*
** qrshl_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	uqrshl	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (qrshl_u64_z_tied1, svuint64_t, svint64_t,
	     z0 = svqrshl_u64_z (p0, z0, z4),
	     z0 = svqrshl_z (p0, z0, z4))

/*
** qrshl_u64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	uqrshlr	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z_REV (qrshl_u64_z_tied2, svuint64_t, svint64_t,
		 z0_res = svqrshl_u64_z (p0, z4, z0),
		 z0_res = svqrshl_z (p0, z4, z0))

/*
** qrshl_u64_z_untied:
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	uqrshl	z0\.d, p0/m, z0\.d, z4\.d
** |
**	movprfx	z0\.d, p0/z, z4\.d
**	uqrshlr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qrshl_u64_z_untied, svuint64_t, svint64_t,
	     z0 = svqrshl_u64_z (p0, z1, z4),
	     z0 = svqrshl_z (p0, z1, z4))

/*
** qrshl_x0_u64_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	uqrshl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_x0_u64_z_tied1, svuint64_t, int64_t,
		 z0 = svqrshl_n_u64_z (p0, z0, x0),
		 z0 = svqrshl_z (p0, z0, x0))

/*
** qrshl_x0_u64_z_untied:
**	mov	(z[0-9]+\.d), x0
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	uqrshl	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	uqrshlr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_ZX (qrshl_x0_u64_z_untied, svuint64_t, int64_t,
		 z0 = svqrshl_n_u64_z (p0, z1, x0),
		 z0 = svqrshl_z (p0, z1, x0))

/*
** qrshl_m64_u64_z:
**	movprfx	z0\.d, p0/z, z0\.d
**	urshr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (qrshl_m64_u64_z, svuint64_t,
		z0 = svqrshl_n_u64_z (p0, z0, -64),
		z0 = svqrshl_z (p0, z0, -64))

/*
** qrshl_m2_u64_z:
**	movprfx	z0\.d, p0/z, z0\.d
**	urshr	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_m2_u64_z, svuint64_t,
		z0 = svqrshl_n_u64_z (p0, z0, -2),
		z0 = svqrshl_z (p0, z0, -2))

/*
** qrshl_m1_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	urshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_u64_z_tied1, svuint64_t,
		z0 = svqrshl_n_u64_z (p0, z0, -1),
		z0 = svqrshl_z (p0, z0, -1))

/*
** qrshl_m1_u64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	urshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_u64_z_untied, svuint64_t,
		z0 = svqrshl_n_u64_z (p0, z1, -1),
		z0 = svqrshl_z (p0, z1, -1))

/*
** qrshl_1_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	uqshl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_u64_z_tied1, svuint64_t,
		z0 = svqrshl_n_u64_z (p0, z0, 1),
		z0 = svqrshl_z (p0, z0, 1))

/*
** qrshl_1_u64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	uqshl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_u64_z_untied, svuint64_t,
		z0 = svqrshl_n_u64_z (p0, z1, 1),
		z0 = svqrshl_z (p0, z1, 1))

/*
** qrshl_2_u64_z:
**	movprfx	z0\.d, p0/z, z0\.d
**	uqshl	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_2_u64_z, svuint64_t,
		z0 = svqrshl_n_u64_z (p0, z0, 2),
		z0 = svqrshl_z (p0, z0, 2))

/*
** qrshl_63_u64_z:
**	movprfx	z0\.d, p0/z, z0\.d
**	uqshl	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (qrshl_63_u64_z, svuint64_t,
		z0 = svqrshl_n_u64_z (p0, z0, 63),
		z0 = svqrshl_z (p0, z0, 63))

/*
** qrshl_u64_x_tied1:
**	uqrshl	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (qrshl_u64_x_tied1, svuint64_t, svint64_t,
	     z0 = svqrshl_u64_x (p0, z0, z4),
	     z0 = svqrshl_x (p0, z0, z4))

/*
** qrshl_u64_x_tied2:
**	uqrshlr	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z_REV (qrshl_u64_x_tied2, svuint64_t, svint64_t,
		 z0_res = svqrshl_u64_x (p0, z4, z0),
		 z0_res = svqrshl_x (p0, z4, z0))

/*
** qrshl_u64_x_untied:
** (
**	movprfx	z0, z1
**	uqrshl	z0\.d, p0/m, z0\.d, z4\.d
** |
**	movprfx	z0, z4
**	uqrshlr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qrshl_u64_x_untied, svuint64_t, svint64_t,
	     z0 = svqrshl_u64_x (p0, z1, z4),
	     z0 = svqrshl_x (p0, z1, z4))

/*
** qrshl_x0_u64_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	uqrshl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qrshl_x0_u64_x_tied1, svuint64_t, int64_t,
		 z0 = svqrshl_n_u64_x (p0, z0, x0),
		 z0 = svqrshl_x (p0, z0, x0))

/*
** qrshl_x0_u64_x_untied:
**	mov	z0\.d, x0
**	uqrshlr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_ZX (qrshl_x0_u64_x_untied, svuint64_t, int64_t,
		 z0 = svqrshl_n_u64_x (p0, z1, x0),
		 z0 = svqrshl_x (p0, z1, x0))

/*
** qrshl_m64_u64_x:
**	urshr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (qrshl_m64_u64_x, svuint64_t,
		z0 = svqrshl_n_u64_x (p0, z0, -64),
		z0 = svqrshl_x (p0, z0, -64))

/*
** qrshl_m2_u64_x:
**	urshr	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_m2_u64_x, svuint64_t,
		z0 = svqrshl_n_u64_x (p0, z0, -2),
		z0 = svqrshl_x (p0, z0, -2))

/*
** qrshl_m1_u64_x_tied1:
**	urshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_u64_x_tied1, svuint64_t,
		z0 = svqrshl_n_u64_x (p0, z0, -1),
		z0 = svqrshl_x (p0, z0, -1))

/*
** qrshl_m1_u64_x_untied:
**	movprfx	z0, z1
**	urshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_m1_u64_x_untied, svuint64_t,
		z0 = svqrshl_n_u64_x (p0, z1, -1),
		z0 = svqrshl_x (p0, z1, -1))

/*
** qrshl_1_u64_x_tied1:
**	uqshl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_u64_x_tied1, svuint64_t,
		z0 = svqrshl_n_u64_x (p0, z0, 1),
		z0 = svqrshl_x (p0, z0, 1))

/*
** qrshl_1_u64_x_untied:
**	movprfx	z0, z1
**	uqshl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qrshl_1_u64_x_untied, svuint64_t,
		z0 = svqrshl_n_u64_x (p0, z1, 1),
		z0 = svqrshl_x (p0, z1, 1))

/*
** qrshl_2_u64_x:
**	uqshl	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (qrshl_2_u64_x, svuint64_t,
		z0 = svqrshl_n_u64_x (p0, z0, 2),
		z0 = svqrshl_x (p0, z0, 2))

/*
** qrshl_63_u64_x:
**	uqshl	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (qrshl_63_u64_x, svuint64_t,
		z0 = svqrshl_n_u64_x (p0, z0, 63),
		z0 = svqrshl_x (p0, z0, 63))
