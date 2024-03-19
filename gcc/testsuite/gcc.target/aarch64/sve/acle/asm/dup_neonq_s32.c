/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dup_neonq_s32_z0:
**	dup	z0.q, z4.q\[0\]
**	ret
*/
TEST_DUP_NEONQ (dup_neonq_s32_z0, int32x4_t, svint32_t,
	  z0 = svdup_neonq_s32 (z4),
	  z0 = svdup_neonq (z4))

/*
** dup_neonq_s32_z4:
**	dup	z4.q, z4.q\[0\]
**	ret
*/
TEST_DUP_NEONQ (dup_neonq_s32_z4, int32x4_t, svint32_t,
	  z4_res = svdup_neonq_s32 (z4),
	  z4_res = svdup_neonq (z4))

/*
** dup_neonq_s32_z5:
**	dup	z5.q, z4.q\[0\]
**	ret
*/
TEST_DUP_NEONQ (dup_neonq_s32_z5, int32x4_t, svint32_t,
	  z5_res = svdup_neonq_s32 (z4),
	  z5_res = svdup_neonq (z4))
