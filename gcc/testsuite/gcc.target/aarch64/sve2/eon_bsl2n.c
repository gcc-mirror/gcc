/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_neon.h>
#include <arm_sve.h>

#define EON(x, y)   (~((x) ^ (y)))

/*
** eon_d:
** 	bsl2n	z0.d, z0.d, z0.d, z1.d
** 	ret
*/
uint32x2_t eon_d(uint32x2_t a, uint32x2_t b) { return EON(a, b); }

/*
** eon_d_mp:
**	movprfx	z0, z1
** 	bsl2n	z0.d, z0.d, z1.d, z2.d
** 	ret
*/
uint32x2_t eon_d_mp(uint32x2_t c, uint32x2_t a, uint32x2_t b) { return EON(a, b); }

/*
** eon_q:
** 	bsl2n	z0.d, z0.d, z0.d, z1.d
** 	ret
*/
uint64x2_t eon_q(uint64x2_t a, uint64x2_t b) { return EON(a, b); }

/*
** eon_q_mp:
**	movprfx	z0, z1
** 	bsl2n	z0.d, z0.d, z1.d, z2.d
** 	ret
*/
uint64x2_t eon_q_mp(uint64x2_t c, uint64x2_t a, uint64x2_t b) { return EON(a, b); }

/*
** eon_z:
** 	bsl2n	z0.d, z0.d, z0.d, z1.d
** 	ret
*/
svuint64_t eon_z(svuint64_t a, svuint64_t b) { return EON(a, b); }

/*
** eon_z_mp:
**	movprfx	z0, z1
** 	bsl2n	z0.d, z0.d, z1.d, z2.d
** 	ret
*/
svuint64_t eon_z_mp(svuint64_t c, svuint64_t a, svuint64_t b) { return EON(a, b); }
