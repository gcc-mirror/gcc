/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshl_s64_m_tied1:
**	sqshl	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (qshl_s64_m_tied1, svint64_t, svint64_t,
	     z0 = svqshl_s64_m (p0, z0, z4),
	     z0 = svqshl_m (p0, z0, z4))

/*
** qshl_s64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z4
**	sqshl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_DUAL_Z_REV (qshl_s64_m_tied2, svint64_t, svint64_t,
		 z0_res = svqshl_s64_m (p0, z4, z0),
		 z0_res = svqshl_m (p0, z4, z0))

/*
** qshl_s64_m_untied:
**	movprfx	z0, z1
**	sqshl	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (qshl_s64_m_untied, svint64_t, svint64_t,
	     z0 = svqshl_s64_m (p0, z1, z4),
	     z0 = svqshl_m (p0, z1, z4))

/*
** qshl_x0_s64_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	sqshl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_x0_s64_m_tied1, svint64_t, int64_t,
		 z0 = svqshl_n_s64_m (p0, z0, x0),
		 z0 = svqshl_m (p0, z0, x0))

/*
** qshl_x0_s64_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	sqshl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_x0_s64_m_untied, svint64_t, int64_t,
		 z0 = svqshl_n_s64_m (p0, z1, x0),
		 z0 = svqshl_m (p0, z1, x0))

/*
** qshl_m64_s64_m:
**	asr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (qshl_m64_s64_m, svint64_t,
		z0 = svqshl_n_s64_m (p0, z0, -64),
		z0 = svqshl_m (p0, z0, -64))

/*
** qshl_m2_s64_m:
**	asr	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_m2_s64_m, svint64_t,
		z0 = svqshl_n_s64_m (p0, z0, -2),
		z0 = svqshl_m (p0, z0, -2))

/*
** qshl_m1_s64_m_tied1:
**	asr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_s64_m_tied1, svint64_t,
		z0 = svqshl_n_s64_m (p0, z0, -1),
		z0 = svqshl_m (p0, z0, -1))

/*
** qshl_m1_s64_m_untied:
**	movprfx	z0, z1
**	asr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_s64_m_untied, svint64_t,
		z0 = svqshl_n_s64_m (p0, z1, -1),
		z0 = svqshl_m (p0, z1, -1))

/*
** qshl_1_s64_m_tied1:
**	sqshl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_s64_m_tied1, svint64_t,
		z0 = svqshl_n_s64_m (p0, z0, 1),
		z0 = svqshl_m (p0, z0, 1))

/*
** qshl_1_s64_m_untied:
**	movprfx	z0, z1
**	sqshl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_s64_m_untied, svint64_t,
		z0 = svqshl_n_s64_m (p0, z1, 1),
		z0 = svqshl_m (p0, z1, 1))

/*
** qshl_2_s64_m:
**	sqshl	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_2_s64_m, svint64_t,
		z0 = svqshl_n_s64_m (p0, z0, 2),
		z0 = svqshl_m (p0, z0, 2))

/*
** qshl_63_s64_m:
**	sqshl	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (qshl_63_s64_m, svint64_t,
		z0 = svqshl_n_s64_m (p0, z0, 63),
		z0 = svqshl_m (p0, z0, 63))

/*
** qshl_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	sqshl	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (qshl_s64_z_tied1, svint64_t, svint64_t,
	     z0 = svqshl_s64_z (p0, z0, z4),
	     z0 = svqshl_z (p0, z0, z4))

/*
** qshl_s64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	sqshlr	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z_REV (qshl_s64_z_tied2, svint64_t, svint64_t,
		 z0_res = svqshl_s64_z (p0, z4, z0),
		 z0_res = svqshl_z (p0, z4, z0))

/*
** qshl_s64_z_untied:
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	sqshl	z0\.d, p0/m, z0\.d, z4\.d
** |
**	movprfx	z0\.d, p0/z, z4\.d
**	sqshlr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qshl_s64_z_untied, svint64_t, svint64_t,
	     z0 = svqshl_s64_z (p0, z1, z4),
	     z0 = svqshl_z (p0, z1, z4))

/*
** qshl_x0_s64_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	sqshl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_x0_s64_z_tied1, svint64_t, int64_t,
		 z0 = svqshl_n_s64_z (p0, z0, x0),
		 z0 = svqshl_z (p0, z0, x0))

/*
** qshl_x0_s64_z_untied:
**	mov	(z[0-9]+\.d), x0
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	sqshl	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	sqshlr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_ZX (qshl_x0_s64_z_untied, svint64_t, int64_t,
		 z0 = svqshl_n_s64_z (p0, z1, x0),
		 z0 = svqshl_z (p0, z1, x0))

/*
** qshl_m64_s64_z:
**	movprfx	z0\.d, p0/z, z0\.d
**	asr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (qshl_m64_s64_z, svint64_t,
		z0 = svqshl_n_s64_z (p0, z0, -64),
		z0 = svqshl_z (p0, z0, -64))

/*
** qshl_m2_s64_z:
**	movprfx	z0\.d, p0/z, z0\.d
**	asr	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_m2_s64_z, svint64_t,
		z0 = svqshl_n_s64_z (p0, z0, -2),
		z0 = svqshl_z (p0, z0, -2))

/*
** qshl_m1_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	asr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_s64_z_tied1, svint64_t,
		z0 = svqshl_n_s64_z (p0, z0, -1),
		z0 = svqshl_z (p0, z0, -1))

/*
** qshl_m1_s64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	asr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_s64_z_untied, svint64_t,
		z0 = svqshl_n_s64_z (p0, z1, -1),
		z0 = svqshl_z (p0, z1, -1))

/*
** qshl_1_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	sqshl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_s64_z_tied1, svint64_t,
		z0 = svqshl_n_s64_z (p0, z0, 1),
		z0 = svqshl_z (p0, z0, 1))

/*
** qshl_1_s64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	sqshl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_s64_z_untied, svint64_t,
		z0 = svqshl_n_s64_z (p0, z1, 1),
		z0 = svqshl_z (p0, z1, 1))

/*
** qshl_2_s64_z:
**	movprfx	z0\.d, p0/z, z0\.d
**	sqshl	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_2_s64_z, svint64_t,
		z0 = svqshl_n_s64_z (p0, z0, 2),
		z0 = svqshl_z (p0, z0, 2))

/*
** qshl_63_s64_z:
**	movprfx	z0\.d, p0/z, z0\.d
**	sqshl	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (qshl_63_s64_z, svint64_t,
		z0 = svqshl_n_s64_z (p0, z0, 63),
		z0 = svqshl_z (p0, z0, 63))

/*
** qshl_s64_x_tied1:
**	sqshl	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (qshl_s64_x_tied1, svint64_t, svint64_t,
	     z0 = svqshl_s64_x (p0, z0, z4),
	     z0 = svqshl_x (p0, z0, z4))

/*
** qshl_s64_x_tied2:
**	sqshlr	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z_REV (qshl_s64_x_tied2, svint64_t, svint64_t,
		 z0_res = svqshl_s64_x (p0, z4, z0),
		 z0_res = svqshl_x (p0, z4, z0))

/*
** qshl_s64_x_untied:
** (
**	movprfx	z0, z1
**	sqshl	z0\.d, p0/m, z0\.d, z4\.d
** |
**	movprfx	z0, z4
**	sqshlr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qshl_s64_x_untied, svint64_t, svint64_t,
	     z0 = svqshl_s64_x (p0, z1, z4),
	     z0 = svqshl_x (p0, z1, z4))

/*
** qshl_x0_s64_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	sqshl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qshl_x0_s64_x_tied1, svint64_t, int64_t,
		 z0 = svqshl_n_s64_x (p0, z0, x0),
		 z0 = svqshl_x (p0, z0, x0))

/*
** qshl_x0_s64_x_untied:
**	mov	z0\.d, x0
**	sqshlr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_ZX (qshl_x0_s64_x_untied, svint64_t, int64_t,
		 z0 = svqshl_n_s64_x (p0, z1, x0),
		 z0 = svqshl_x (p0, z1, x0))

/*
** qshl_m64_s64_x:
**	asr	z0\.d, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (qshl_m64_s64_x, svint64_t,
		z0 = svqshl_n_s64_x (p0, z0, -64),
		z0 = svqshl_x (p0, z0, -64))

/*
** qshl_m2_s64_x:
**	asr	z0\.d, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_m2_s64_x, svint64_t,
		z0 = svqshl_n_s64_x (p0, z0, -2),
		z0 = svqshl_x (p0, z0, -2))

/*
** qshl_m1_s64_x_tied1:
**	asr	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_s64_x_tied1, svint64_t,
		z0 = svqshl_n_s64_x (p0, z0, -1),
		z0 = svqshl_x (p0, z0, -1))

/*
** qshl_m1_s64_x_untied:
**	asr	z0\.d, z1\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_m1_s64_x_untied, svint64_t,
		z0 = svqshl_n_s64_x (p0, z1, -1),
		z0 = svqshl_x (p0, z1, -1))

/*
** qshl_1_s64_x_tied1:
**	sqshl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_s64_x_tied1, svint64_t,
		z0 = svqshl_n_s64_x (p0, z0, 1),
		z0 = svqshl_x (p0, z0, 1))

/*
** qshl_1_s64_x_untied:
**	movprfx	z0, z1
**	sqshl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (qshl_1_s64_x_untied, svint64_t,
		z0 = svqshl_n_s64_x (p0, z1, 1),
		z0 = svqshl_x (p0, z1, 1))

/*
** qshl_2_s64_x:
**	sqshl	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (qshl_2_s64_x, svint64_t,
		z0 = svqshl_n_s64_x (p0, z0, 2),
		z0 = svqshl_x (p0, z0, 2))

/*
** qshl_63_s64_x:
**	sqshl	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (qshl_63_s64_x, svint64_t,
		z0 = svqshl_n_s64_x (p0, z0, 63),
		z0 = svqshl_x (p0, z0, 63))
