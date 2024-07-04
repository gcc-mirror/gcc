/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** set_neonq_bf16_z24:
**	ptrue	(p[0-9]+).h, vl8
**	sel	z24.h, \1, z0.h, z4.h
**	ret
*/
TEST_SET_NEONQ (set_neonq_bf16_z24, svbfloat16_t, bfloat16x8_t,
	  z24 = svset_neonq_bf16 (z4, z0),
	  z24 = svset_neonq (z4, z0))

/*
** set_neonq_bf16_z4:
**	ptrue	(p[0-9]+).h, vl8
**	sel	z4.h, \1, z0.h, z4.h
**	ret
*/
TEST_SET_NEONQ (set_neonq_bf16_z4, svbfloat16_t, bfloat16x8_t,
	  z4_res = svset_neonq_bf16 (z4, z0),
	  z4_res = svset_neonq (z4, z0))