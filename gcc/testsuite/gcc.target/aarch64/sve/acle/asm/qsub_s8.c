/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qsub_s8_tied1:
**	sqsub	z0\.b, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (qsub_s8_tied1, svint8_t,
		z0 = svqsub_s8 (z0, z1),
		z0 = svqsub (z0, z1))

/*
** qsub_s8_tied2:
**	sqsub	z0\.b, z1\.b, z0\.b
**	ret
*/
TEST_UNIFORM_Z (qsub_s8_tied2, svint8_t,
		z0 = svqsub_s8 (z1, z0),
		z0 = svqsub (z1, z0))

/*
** qsub_s8_untied:
**	sqsub	z0\.b, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (qsub_s8_untied, svint8_t,
		z0 = svqsub_s8 (z1, z2),
		z0 = svqsub (z1, z2))

/*
** qsub_w0_s8_tied1:
**	mov	(z[0-9]+\.b), w0
**	sqsub	z0\.b, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_s8_tied1, svint8_t, int8_t,
		 z0 = svqsub_n_s8 (z0, x0),
		 z0 = svqsub (z0, x0))

/*
** qsub_w0_s8_untied:
**	mov	(z[0-9]+\.b), w0
**	sqsub	z0\.b, z1\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_s8_untied, svint8_t, int8_t,
		 z0 = svqsub_n_s8 (z1, x0),
		 z0 = svqsub (z1, x0))

/*
** qsub_1_s8_tied1:
**	sqsub	z0\.b, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_s8_tied1, svint8_t,
		z0 = svqsub_n_s8 (z0, 1),
		z0 = svqsub (z0, 1))

/*
** qsub_1_s8_untied:
**	movprfx	z0, z1
**	sqsub	z0\.b, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_s8_untied, svint8_t,
		z0 = svqsub_n_s8 (z1, 1),
		z0 = svqsub (z1, 1))

/*
** qsub_127_s8:
**	sqsub	z0\.b, z0\.b, #127
**	ret
*/
TEST_UNIFORM_Z (qsub_127_s8, svint8_t,
		z0 = svqsub_n_s8 (z0, 127),
		z0 = svqsub (z0, 127))

/*
** qsub_128_s8:
**	sqadd	z0\.b, z0\.b, #128
**	ret
*/
TEST_UNIFORM_Z (qsub_128_s8, svint8_t,
		z0 = svqsub_n_s8 (z0, 128),
		z0 = svqsub (z0, 128))

/*
** qsub_255_s8:
**	sqadd	z0\.b, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_255_s8, svint8_t,
		z0 = svqsub_n_s8 (z0, 255),
		z0 = svqsub (z0, 255))

/*
** qsub_m1_s8:
**	sqadd	z0\.b, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_m1_s8, svint8_t,
		z0 = svqsub_n_s8 (z0, -1),
		z0 = svqsub (z0, -1))

/*
** qsub_m127_s8:
**	sqadd	z0\.b, z0\.b, #127
**	ret
*/
TEST_UNIFORM_Z (qsub_m127_s8, svint8_t,
		z0 = svqsub_n_s8 (z0, -127),
		z0 = svqsub (z0, -127))

/*
** qsub_m128_s8:
**	sqadd	z0\.b, z0\.b, #128
**	ret
*/
TEST_UNIFORM_Z (qsub_m128_s8, svint8_t,
		z0 = svqsub_n_s8 (z0, -128),
		z0 = svqsub (z0, -128))
