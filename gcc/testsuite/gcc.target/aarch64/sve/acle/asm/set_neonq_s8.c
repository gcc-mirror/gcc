/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** set_neonq_s8_z24:
**	ptrue	(p[0-9]+).b, vl16
**	sel	z24.b, \1, z0.b, z4.b
**	ret
*/
TEST_SET_NEONQ (set_neonq_s8_z24, svint8_t, int8x16_t,
	  z24 = svset_neonq_s8 (z4, z0),
	  z24 = svset_neonq (z4, z0))

/*
** set_neonq_s8_z4:
**	ptrue	(p[0-9]+).b, vl16
**	sel	z4.b, \1, z0.b, z4.b
**	ret
*/
TEST_SET_NEONQ (set_neonq_s8_z4, svint8_t, int8x16_t,
	  z4_res = svset_neonq_s8 (z4, z0),
	  z4_res = svset_neonq (z4, z0))