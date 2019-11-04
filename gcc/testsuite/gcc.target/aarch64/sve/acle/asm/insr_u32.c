/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** insr_w0_u32_tied1:
**	insr	z0\.s, w0
**	ret
*/
TEST_UNIFORM_ZX (insr_w0_u32_tied1, svuint32_t, uint32_t,
		 z0 = svinsr_n_u32 (z0, x0),
		 z0 = svinsr (z0, x0))

/*
** insr_w0_u32_untied:
**	movprfx	z0, z1
**	insr	z0\.s, w0
**	ret
*/
TEST_UNIFORM_ZX (insr_w0_u32_untied, svuint32_t, uint32_t,
		 z0 = svinsr_n_u32 (z1, x0),
		 z0 = svinsr (z1, x0))

/*
** insr_0_u32_tied1:
**	insr	z0\.s, wzr
**	ret
*/
TEST_UNIFORM_Z (insr_0_u32_tied1, svuint32_t,
		z0 = svinsr_n_u32 (z0, 0),
		z0 = svinsr (z0, 0))

/*
** insr_0_u32_untied:
**	movprfx	z0, z1
**	insr	z0\.s, wzr
**	ret
*/
TEST_UNIFORM_Z (insr_0_u32_untied, svuint32_t,
		z0 = svinsr_n_u32 (z1, 0),
		z0 = svinsr (z1, 0))

/*
** insr_1_u32:
** (
**	mov	(w[0-9]+), #?1
**	insr	z0\.s, \1
** |
**	movi	v([0-9]+)\.2s, 0x1
**	insr	z0\.s, s\2
** )
**	ret
*/
TEST_UNIFORM_Z (insr_1_u32, svuint32_t,
		z0 = svinsr_n_u32 (z0, 1),
		z0 = svinsr (z0, 1))
