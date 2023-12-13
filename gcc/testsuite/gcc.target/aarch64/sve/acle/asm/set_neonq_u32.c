/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** set_neonq_u32_z24:
**	ptrue	(p[0-9]+).s, vl4
**	sel	z24.s, \1, z0.s, z4.s
**	ret
*/
TEST_SET_NEONQ (set_neonq_u32_z24, svuint32_t, uint32x4_t,
	  z24 = svset_neonq_u32 (z4, z0),
	  z24 = svset_neonq (z4, z0))

/*
** set_neonq_u32_z4:
**	ptrue	(p[0-9]+).s, vl4
**	sel	z4.s, \1, z0.s, z4.s
**	ret
*/
TEST_SET_NEONQ (set_neonq_u32_z4, svuint32_t, uint32x4_t,
	  z4_res = svset_neonq_u32 (z4, z0),
	  z4_res = svset_neonq (z4, z0))