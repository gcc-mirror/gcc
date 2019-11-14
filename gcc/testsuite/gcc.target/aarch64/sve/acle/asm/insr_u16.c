/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** insr_w0_u16_tied1:
**	insr	z0\.h, w0
**	ret
*/
TEST_UNIFORM_ZX (insr_w0_u16_tied1, svuint16_t, uint16_t,
		 z0 = svinsr_n_u16 (z0, x0),
		 z0 = svinsr (z0, x0))

/*
** insr_w0_u16_untied:
**	movprfx	z0, z1
**	insr	z0\.h, w0
**	ret
*/
TEST_UNIFORM_ZX (insr_w0_u16_untied, svuint16_t, uint16_t,
		 z0 = svinsr_n_u16 (z1, x0),
		 z0 = svinsr (z1, x0))

/*
** insr_0_u16_tied1:
**	insr	z0\.h, wzr
**	ret
*/
TEST_UNIFORM_Z (insr_0_u16_tied1, svuint16_t,
		z0 = svinsr_n_u16 (z0, 0),
		z0 = svinsr (z0, 0))

/*
** insr_0_u16_untied:
**	movprfx	z0, z1
**	insr	z0\.h, wzr
**	ret
*/
TEST_UNIFORM_Z (insr_0_u16_untied, svuint16_t,
		z0 = svinsr_n_u16 (z1, 0),
		z0 = svinsr (z1, 0))

/*
** insr_1_u16:
** (
**	mov	(w[0-9]+), #?1
**	insr	z0\.h, \1
** |
**	movi	v([0-9]+)\.4h, 0x1
**	insr	z0\.h, h\2
** )
**	ret
*/
TEST_UNIFORM_Z (insr_1_u16, svuint16_t,
		z0 = svinsr_n_u16 (z0, 1),
		z0 = svinsr (z0, 1))
