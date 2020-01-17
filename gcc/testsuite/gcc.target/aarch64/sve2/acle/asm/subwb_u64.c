/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** subwb_u64_tied1:
**	usubwb	z0\.d, z0\.d, z4\.s
**	ret
*/
TEST_DUAL_Z (subwb_u64_tied1, svuint64_t, svuint32_t,
	     z0 = svsubwb_u64 (z0, z4),
	     z0 = svsubwb (z0, z4))

/*
** subwb_u64_tied2:
**	usubwb	z0\.d, z4\.d, z0\.s
**	ret
*/
TEST_DUAL_Z_REV (subwb_u64_tied2, svuint64_t, svuint32_t,
		 z0_res = svsubwb_u64 (z4, z0),
		 z0_res = svsubwb (z4, z0))

/*
** subwb_u64_untied:
**	usubwb	z0\.d, z1\.d, z4\.s
**	ret
*/
TEST_DUAL_Z (subwb_u64_untied, svuint64_t, svuint32_t,
	     z0 = svsubwb_u64 (z1, z4),
	     z0 = svsubwb (z1, z4))

/*
** subwb_w0_u64_tied1:
**	mov	(z[0-9]+\.s), w0
**	usubwb	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (subwb_w0_u64_tied1, svuint64_t, uint32_t,
		 z0 = svsubwb_n_u64 (z0, x0),
		 z0 = svsubwb (z0, x0))

/*
** subwb_w0_u64_untied:
**	mov	(z[0-9]+\.s), w0
**	usubwb	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (subwb_w0_u64_untied, svuint64_t, uint32_t,
		 z0 = svsubwb_n_u64 (z1, x0),
		 z0 = svsubwb (z1, x0))

/*
** subwb_11_u64_tied1:
**	mov	(z[0-9]+\.s), #11
**	usubwb	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (subwb_11_u64_tied1, svuint64_t,
		z0 = svsubwb_n_u64 (z0, 11),
		z0 = svsubwb (z0, 11))

/*
** subwb_11_u64_untied:
**	mov	(z[0-9]+\.s), #11
**	usubwb	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (subwb_11_u64_untied, svuint64_t,
		z0 = svsubwb_n_u64 (z1, 11),
		z0 = svsubwb (z1, 11))
