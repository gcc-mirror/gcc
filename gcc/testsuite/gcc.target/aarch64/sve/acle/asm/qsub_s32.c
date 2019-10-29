/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qsub_s32_tied1:
**	sqsub	z0\.s, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (qsub_s32_tied1, svint32_t,
		z0 = svqsub_s32 (z0, z1),
		z0 = svqsub (z0, z1))

/*
** qsub_s32_tied2:
**	sqsub	z0\.s, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qsub_s32_tied2, svint32_t,
		z0 = svqsub_s32 (z1, z0),
		z0 = svqsub (z1, z0))

/*
** qsub_s32_untied:
**	sqsub	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (qsub_s32_untied, svint32_t,
		z0 = svqsub_s32 (z1, z2),
		z0 = svqsub (z1, z2))

/*
** qsub_w0_s32_tied1:
**	mov	(z[0-9]+\.s), w0
**	sqsub	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_s32_tied1, svint32_t, int32_t,
		 z0 = svqsub_n_s32 (z0, x0),
		 z0 = svqsub (z0, x0))

/*
** qsub_w0_s32_untied:
**	mov	(z[0-9]+\.s), w0
**	sqsub	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_s32_untied, svint32_t, int32_t,
		 z0 = svqsub_n_s32 (z1, x0),
		 z0 = svqsub (z1, x0))

/*
** qsub_1_s32_tied1:
**	sqsub	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_s32_tied1, svint32_t,
		z0 = svqsub_n_s32 (z0, 1),
		z0 = svqsub (z0, 1))

/*
** qsub_1_s32_untied:
**	movprfx	z0, z1
**	sqsub	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_s32_untied, svint32_t,
		z0 = svqsub_n_s32 (z1, 1),
		z0 = svqsub (z1, 1))

/*
** qsub_127_s32:
**	sqsub	z0\.s, z0\.s, #127
**	ret
*/
TEST_UNIFORM_Z (qsub_127_s32, svint32_t,
		z0 = svqsub_n_s32 (z0, 127),
		z0 = svqsub (z0, 127))

/*
** qsub_128_s32:
**	sqsub	z0\.s, z0\.s, #128
**	ret
*/
TEST_UNIFORM_Z (qsub_128_s32, svint32_t,
		z0 = svqsub_n_s32 (z0, 128),
		z0 = svqsub (z0, 128))

/*
** qsub_255_s32:
**	sqsub	z0\.s, z0\.s, #255
**	ret
*/
TEST_UNIFORM_Z (qsub_255_s32, svint32_t,
		z0 = svqsub_n_s32 (z0, 255),
		z0 = svqsub (z0, 255))

/*
** qsub_m1_s32:
**	sqadd	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_m1_s32, svint32_t,
		z0 = svqsub_n_s32 (z0, -1),
		z0 = svqsub (z0, -1))

/*
** qsub_m127_s32:
**	sqadd	z0\.s, z0\.s, #127
**	ret
*/
TEST_UNIFORM_Z (qsub_m127_s32, svint32_t,
		z0 = svqsub_n_s32 (z0, -127),
		z0 = svqsub (z0, -127))

/*
** qsub_m128_s32:
**	sqadd	z0\.s, z0\.s, #128
**	ret
*/
TEST_UNIFORM_Z (qsub_m128_s32, svint32_t,
		z0 = svqsub_n_s32 (z0, -128),
		z0 = svqsub (z0, -128))
