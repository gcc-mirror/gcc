/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshl_u64_m_tied1:
**	uqshl	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (qshl_u64_m_tied1, svuint64_t, svint64_t,
	     z0 = svqshl_u64_m (p0, z0, z4),
	     z0 = svqshl_m (p0, z0, z4))

/*
** qshl_u64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z4
**	uqshl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_DUAL_Z_REV (qshl_u64_m_tied2, svuint64_t, svint64_t,
		 z0_res = svqshl_u64_m (p0, z4, z0),
		 z0_res = svqshl_m (p0, z4, z0))

/*
** qshl_u64_m_untied:
**	movprfx	z0, z1
**	uqshl	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (qshl_u64_m_untied, svuint64_t, svint64_t,
	     z0 = svqshl_u64_m (p0, z1, z4),
	     z0 = svqshl_m (p0, z1, z4))

/*
** qshl_x0_u64_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	uqshl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_x0_u64_m_tied1, svuint64_t, int64_t,
		 z0 = svqshl_n_u64_m (p0, z0, x0),
		 z0 = svqshl_m (p0, z0, x0))

/*
** qshl_x0_u64_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	uqshl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_x0_u64_m_untied, svuint64_t, int64_t,
		 z0 = svqshl_n_u64_m (p0, z1, x0),
		 z0 = svqshl_m (p0, z1, x0))

/*
** qshl_m64_u64_m:
**	lsr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (qshl_m64_u64_m, svuint64_t,
		z0 = svqshl_n_u64_m (p0, z0, -64),
		z0 = svqshl_m (p0, z0, -64))

/*
** qshl_m2_u64_m:
**	lsr	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_m2_u64_m, svuint64_t,
		z0 = svqshl_n_u64_m (p0, z0, -2),
		z0 = svqshl_m (p0, z0, -2))

/*
** qshl_m1_u64_m_tied1:
**	lsr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_u64_m_tied1, svuint64_t,
		z0 = svqshl_n_u64_m (p0, z0, -1),
		z0 = svqshl_m (p0, z0, -1))

/*
** qshl_m1_u64_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_u64_m_untied, svuint64_t,
		z0 = svqshl_n_u64_m (p0, z1, -1),
		z0 = svqshl_m (p0, z1, -1))

/*
** qshl_1_u64_m_tied1:
**	uqshl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_u64_m_tied1, svuint64_t,
		z0 = svqshl_n_u64_m (p0, z0, 1),
		z0 = svqshl_m (p0, z0, 1))

/*
** qshl_1_u64_m_untied:
**	movprfx	z0, z1
**	uqshl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_u64_m_untied, svuint64_t,
		z0 = svqshl_n_u64_m (p0, z1, 1),
		z0 = svqshl_m (p0, z1, 1))

/*
** qshl_2_u64_m:
**	uqshl	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_2_u64_m, svuint64_t,
		z0 = svqshl_n_u64_m (p0, z0, 2),
		z0 = svqshl_m (p0, z0, 2))

/*
** qshl_63_u64_m:
**	uqshl	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (qshl_63_u64_m, svuint64_t,
		z0 = svqshl_n_u64_m (p0, z0, 63),
		z0 = svqshl_m (p0, z0, 63))

/*
** qshl_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	uqshl	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (qshl_u64_z_tied1, svuint64_t, svint64_t,
	     z0 = svqshl_u64_z (p0, z0, z4),
	     z0 = svqshl_z (p0, z0, z4))

/*
** qshl_u64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	uqshlr	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z_REV (qshl_u64_z_tied2, svuint64_t, svint64_t,
		 z0_res = svqshl_u64_z (p0, z4, z0),
		 z0_res = svqshl_z (p0, z4, z0))

/*
** qshl_u64_z_untied:
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	uqshl	z0\.d, p0/m, z0\.d, z4\.d
** |
**	movprfx	z0\.d, p0/z, z4\.d
**	uqshlr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qshl_u64_z_untied, svuint64_t, svint64_t,
	     z0 = svqshl_u64_z (p0, z1, z4),
	     z0 = svqshl_z (p0, z1, z4))

/*
** qshl_x0_u64_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	uqshl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_x0_u64_z_tied1, svuint64_t, int64_t,
		 z0 = svqshl_n_u64_z (p0, z0, x0),
		 z0 = svqshl_z (p0, z0, x0))

/*
** qshl_x0_u64_z_untied:
**	mov	(z[0-9]+\.d), x0
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	uqshl	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	uqshlr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_ZX (qshl_x0_u64_z_untied, svuint64_t, int64_t,
		 z0 = svqshl_n_u64_z (p0, z1, x0),
		 z0 = svqshl_z (p0, z1, x0))

/*
** qshl_m64_u64_z:
**	movprfx	z0\.d, p0/z, z0\.d
**	lsr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (qshl_m64_u64_z, svuint64_t,
		z0 = svqshl_n_u64_z (p0, z0, -64),
		z0 = svqshl_z (p0, z0, -64))

/*
** qshl_m2_u64_z:
**	movprfx	z0\.d, p0/z, z0\.d
**	lsr	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_m2_u64_z, svuint64_t,
		z0 = svqshl_n_u64_z (p0, z0, -2),
		z0 = svqshl_z (p0, z0, -2))

/*
** qshl_m1_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	lsr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_u64_z_tied1, svuint64_t,
		z0 = svqshl_n_u64_z (p0, z0, -1),
		z0 = svqshl_z (p0, z0, -1))

/*
** qshl_m1_u64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	lsr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_u64_z_untied, svuint64_t,
		z0 = svqshl_n_u64_z (p0, z1, -1),
		z0 = svqshl_z (p0, z1, -1))

/*
** qshl_1_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	uqshl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_u64_z_tied1, svuint64_t,
		z0 = svqshl_n_u64_z (p0, z0, 1),
		z0 = svqshl_z (p0, z0, 1))

/*
** qshl_1_u64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	uqshl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_u64_z_untied, svuint64_t,
		z0 = svqshl_n_u64_z (p0, z1, 1),
		z0 = svqshl_z (p0, z1, 1))

/*
** qshl_2_u64_z:
**	movprfx	z0\.d, p0/z, z0\.d
**	uqshl	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_2_u64_z, svuint64_t,
		z0 = svqshl_n_u64_z (p0, z0, 2),
		z0 = svqshl_z (p0, z0, 2))

/*
** qshl_63_u64_z:
**	movprfx	z0\.d, p0/z, z0\.d
**	uqshl	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (qshl_63_u64_z, svuint64_t,
		z0 = svqshl_n_u64_z (p0, z0, 63),
		z0 = svqshl_z (p0, z0, 63))

/*
** qshl_u64_x_tied1:
**	uqshl	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (qshl_u64_x_tied1, svuint64_t, svint64_t,
	     z0 = svqshl_u64_x (p0, z0, z4),
	     z0 = svqshl_x (p0, z0, z4))

/*
** qshl_u64_x_tied2:
**	uqshlr	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z_REV (qshl_u64_x_tied2, svuint64_t, svint64_t,
		 z0_res = svqshl_u64_x (p0, z4, z0),
		 z0_res = svqshl_x (p0, z4, z0))

/*
** qshl_u64_x_untied:
** (
**	movprfx	z0, z1
**	uqshl	z0\.d, p0/m, z0\.d, z4\.d
** |
**	movprfx	z0, z4
**	uqshlr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qshl_u64_x_untied, svuint64_t, svint64_t,
	     z0 = svqshl_u64_x (p0, z1, z4),
	     z0 = svqshl_x (p0, z1, z4))

/*
** qshl_x0_u64_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	uqshl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_x0_u64_x_tied1, svuint64_t, int64_t,
		 z0 = svqshl_n_u64_x (p0, z0, x0),
		 z0 = svqshl_x (p0, z0, x0))

/*
** qshl_x0_u64_x_untied:
**	mov	z0\.d, x0
**	uqshlr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_ZX (qshl_x0_u64_x_untied, svuint64_t, int64_t,
		 z0 = svqshl_n_u64_x (p0, z1, x0),
		 z0 = svqshl_x (p0, z1, x0))

/*
** qshl_m64_u64_x:
**	lsr	z0\.d, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (qshl_m64_u64_x, svuint64_t,
		z0 = svqshl_n_u64_x (p0, z0, -64),
		z0 = svqshl_x (p0, z0, -64))

/*
** qshl_m2_u64_x:
**	lsr	z0\.d, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_m2_u64_x, svuint64_t,
		z0 = svqshl_n_u64_x (p0, z0, -2),
		z0 = svqshl_x (p0, z0, -2))

/*
** qshl_m1_u64_x_tied1:
**	lsr	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_u64_x_tied1, svuint64_t,
		z0 = svqshl_n_u64_x (p0, z0, -1),
		z0 = svqshl_x (p0, z0, -1))

/*
** qshl_m1_u64_x_untied:
**	lsr	z0\.d, z1\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_u64_x_untied, svuint64_t,
		z0 = svqshl_n_u64_x (p0, z1, -1),
		z0 = svqshl_x (p0, z1, -1))

/*
** qshl_1_u64_x_tied1:
**	uqshl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_u64_x_tied1, svuint64_t,
		z0 = svqshl_n_u64_x (p0, z0, 1),
		z0 = svqshl_x (p0, z0, 1))

/*
** qshl_1_u64_x_untied:
**	movprfx	z0, z1
**	uqshl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_u64_x_untied, svuint64_t,
		z0 = svqshl_n_u64_x (p0, z1, 1),
		z0 = svqshl_x (p0, z1, 1))

/*
** qshl_2_u64_x:
**	uqshl	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_2_u64_x, svuint64_t,
		z0 = svqshl_n_u64_x (p0, z0, 2),
		z0 = svqshl_x (p0, z0, 2))

/*
** qshl_63_u64_x:
**	uqshl	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (qshl_63_u64_x, svuint64_t,
		z0 = svqshl_n_u64_x (p0, z0, 63),
		z0 = svqshl_x (p0, z0, 63))
