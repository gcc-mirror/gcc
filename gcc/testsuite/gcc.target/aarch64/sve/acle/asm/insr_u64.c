/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** insr_x0_u64_tied1:
**	insr	z0\.d, x0
**	ret
*/
TEST_UNIFORM_ZX (insr_x0_u64_tied1, svuint64_t, uint64_t,
		 z0 = svinsr_n_u64 (z0, x0),
		 z0 = svinsr (z0, x0))

/*
** insr_x0_u64_untied:
**	movprfx	z0, z1
**	insr	z0\.d, x0
**	ret
*/
TEST_UNIFORM_ZX (insr_x0_u64_untied, svuint64_t, uint64_t,
		 z0 = svinsr_n_u64 (z1, x0),
		 z0 = svinsr (z1, x0))

/*
** insr_0_u64_tied1:
**	insr	z0\.d, xzr
**	ret
*/
TEST_UNIFORM_Z (insr_0_u64_tied1, svuint64_t,
		z0 = svinsr_n_u64 (z0, 0),
		z0 = svinsr (z0, 0))

/*
** insr_0_u64_untied:
**	movprfx	z0, z1
**	insr	z0\.d, xzr
**	ret
*/
TEST_UNIFORM_Z (insr_0_u64_untied, svuint64_t,
		z0 = svinsr_n_u64 (z1, 0),
		z0 = svinsr (z1, 0))

/*
** insr_1_u64:
** (
**	mov	(x[0-9]+), #?1
**	insr	z0\.d, \1
** |
**	movi	v([0-9]+)\.2d, 0x1
**	insr	z0\.d, d\2
** )
**	ret
*/
TEST_UNIFORM_Z (insr_1_u64, svuint64_t,
		z0 = svinsr_n_u64 (z0, 1),
		z0 = svinsr (z0, 1))
