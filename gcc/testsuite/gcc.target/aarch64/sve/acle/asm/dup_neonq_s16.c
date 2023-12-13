/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dup_neonq_s16_z0:
**	dup	z0.q, z4.q\[0\]
**	ret
*/
TEST_DUP_NEONQ (dup_neonq_s16_z0, int16x8_t, svint16_t,
	  z0 = svdup_neonq_s16 (z4),
	  z0 = svdup_neonq (z4))

/*
** dup_neonq_s16_z4:
**	dup	z4.q, z4.q\[0\]
**	ret
*/
TEST_DUP_NEONQ (dup_neonq_s16_z4, int16x8_t, svint16_t,
	  z4_res = svdup_neonq_s16 (z4),
	  z4_res = svdup_neonq (z4))

/*
** dup_neonq_s16_z5:
**	dup	z5.q, z4.q\[0\]
**	ret
*/
TEST_DUP_NEONQ (dup_neonq_s16_z5, int16x8_t, svint16_t,
	  z5_res = svdup_neonq_s16 (z4),
	  z5_res = svdup_neonq (z4))
