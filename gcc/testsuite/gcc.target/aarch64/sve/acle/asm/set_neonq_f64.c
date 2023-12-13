/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** set_neonq_f64_z24:
**	ptrue	(p[0-9]+).d, vl2
**	sel	z24.d, \1, z0.d, z4.d
**	ret
*/
TEST_SET_NEONQ (set_neonq_f64_z24, svfloat64_t, float64x2_t,
	  z24 = svset_neonq_f64 (z4, z0),
	  z24 = svset_neonq (z4, z0))

/*
** set_neonq_f64_z4:
**	ptrue	(p[0-9]+).d, vl2
**	sel	z4.d, \1, z0.d, z4.d
**	ret
*/
TEST_SET_NEONQ (set_neonq_f64_z4, svfloat64_t, float64x2_t,
	  z4_res = svset_neonq_f64 (z4, z0),
	  z4_res = svset_neonq (z4, z0))