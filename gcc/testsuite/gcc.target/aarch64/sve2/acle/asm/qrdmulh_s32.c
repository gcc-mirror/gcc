/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrdmulh_s32_tied1:
**	sqrdmulh	z0\.s, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (qrdmulh_s32_tied1, svint32_t,
		z0 = svqrdmulh_s32 (z0, z1),
		z0 = svqrdmulh (z0, z1))

/*
** qrdmulh_s32_tied2:
**	sqrdmulh	z0\.s, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qrdmulh_s32_tied2, svint32_t,
		z0 = svqrdmulh_s32 (z1, z0),
		z0 = svqrdmulh (z1, z0))

/*
** qrdmulh_s32_untied:
**	sqrdmulh	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (qrdmulh_s32_untied, svint32_t,
		z0 = svqrdmulh_s32 (z1, z2),
		z0 = svqrdmulh (z1, z2))

/*
** qrdmulh_w0_s32_tied1:
**	mov	(z[0-9]+\.s), w0
**	sqrdmulh	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qrdmulh_w0_s32_tied1, svint32_t, int32_t,
		 z0 = svqrdmulh_n_s32 (z0, x0),
		 z0 = svqrdmulh (z0, x0))

/*
** qrdmulh_w0_s32_untied:
**	mov	(z[0-9]+\.s), w0
**	sqrdmulh	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qrdmulh_w0_s32_untied, svint32_t, int32_t,
		 z0 = svqrdmulh_n_s32 (z1, x0),
		 z0 = svqrdmulh (z1, x0))

/*
** qrdmulh_11_s32_tied1:
**	mov	(z[0-9]+\.s), #11
**	sqrdmulh	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qrdmulh_11_s32_tied1, svint32_t,
		z0 = svqrdmulh_n_s32 (z0, 11),
		z0 = svqrdmulh (z0, 11))

/*
** qrdmulh_11_s32_untied:
**	mov	(z[0-9]+\.s), #11
**	sqrdmulh	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qrdmulh_11_s32_untied, svint32_t,
		z0 = svqrdmulh_n_s32 (z1, 11),
		z0 = svqrdmulh (z1, 11))
