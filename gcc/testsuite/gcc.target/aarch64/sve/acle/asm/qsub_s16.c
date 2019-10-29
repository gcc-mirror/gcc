/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qsub_s16_tied1:
**	sqsub	z0\.h, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_s16_tied1, svint16_t,
		z0 = svqsub_s16 (z0, z1),
		z0 = svqsub (z0, z1))

/*
** qsub_s16_tied2:
**	sqsub	z0\.h, z1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_s16_tied2, svint16_t,
		z0 = svqsub_s16 (z1, z0),
		z0 = svqsub (z1, z0))

/*
** qsub_s16_untied:
**	sqsub	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (qsub_s16_untied, svint16_t,
		z0 = svqsub_s16 (z1, z2),
		z0 = svqsub (z1, z2))

/*
** qsub_w0_s16_tied1:
**	mov	(z[0-9]+\.h), w0
**	sqsub	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_s16_tied1, svint16_t, int16_t,
		 z0 = svqsub_n_s16 (z0, x0),
		 z0 = svqsub (z0, x0))

/*
** qsub_w0_s16_untied:
**	mov	(z[0-9]+\.h), w0
**	sqsub	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_s16_untied, svint16_t, int16_t,
		 z0 = svqsub_n_s16 (z1, x0),
		 z0 = svqsub (z1, x0))

/*
** qsub_1_s16_tied1:
**	sqsub	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_s16_tied1, svint16_t,
		z0 = svqsub_n_s16 (z0, 1),
		z0 = svqsub (z0, 1))

/*
** qsub_1_s16_untied:
**	movprfx	z0, z1
**	sqsub	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_s16_untied, svint16_t,
		z0 = svqsub_n_s16 (z1, 1),
		z0 = svqsub (z1, 1))

/*
** qsub_127_s16:
**	sqsub	z0\.h, z0\.h, #127
**	ret
*/
TEST_UNIFORM_Z (qsub_127_s16, svint16_t,
		z0 = svqsub_n_s16 (z0, 127),
		z0 = svqsub (z0, 127))

/*
** qsub_128_s16:
**	sqsub	z0\.h, z0\.h, #128
**	ret
*/
TEST_UNIFORM_Z (qsub_128_s16, svint16_t,
		z0 = svqsub_n_s16 (z0, 128),
		z0 = svqsub (z0, 128))

/*
** qsub_255_s16:
**	sqsub	z0\.h, z0\.h, #255
**	ret
*/
TEST_UNIFORM_Z (qsub_255_s16, svint16_t,
		z0 = svqsub_n_s16 (z0, 255),
		z0 = svqsub (z0, 255))

/*
** qsub_m1_s16:
**	sqadd	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_m1_s16, svint16_t,
		z0 = svqsub_n_s16 (z0, -1),
		z0 = svqsub (z0, -1))

/*
** qsub_m127_s16:
**	sqadd	z0\.h, z0\.h, #127
**	ret
*/
TEST_UNIFORM_Z (qsub_m127_s16, svint16_t,
		z0 = svqsub_n_s16 (z0, -127),
		z0 = svqsub (z0, -127))

/*
** qsub_m128_s16:
**	sqadd	z0\.h, z0\.h, #128
**	ret
*/
TEST_UNIFORM_Z (qsub_m128_s16, svint16_t,
		z0 = svqsub_n_s16 (z0, -128),
		z0 = svqsub (z0, -128))
