/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** ptrue_pat_pow2_b32:
**	ptrue	p0\.s, pow2
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_pow2_b32,
		p0 = svptrue_pat_b32 (SV_POW2),
		p0 = svptrue_pat_b32 (SV_POW2))

/*
** ptrue_pat_vl1_b32:
**	ptrue	p0\.[bhsd], vl1
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_vl1_b32,
		p0 = svptrue_pat_b32 (SV_VL1),
		p0 = svptrue_pat_b32 (SV_VL1))

/*
** ptrue_pat_vl2_b32:
**	ptrue	p0\.s, vl2
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_vl2_b32,
		p0 = svptrue_pat_b32 (SV_VL2),
		p0 = svptrue_pat_b32 (SV_VL2))

/*
** ptrue_pat_vl3_b32:
**	ptrue	p0\.s, vl3
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_vl3_b32,
		p0 = svptrue_pat_b32 (SV_VL3),
		p0 = svptrue_pat_b32 (SV_VL3))

/*
** ptrue_pat_vl4_b32:
**	ptrue	p0\.s, vl4
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_vl4_b32,
		p0 = svptrue_pat_b32 (SV_VL4),
		p0 = svptrue_pat_b32 (SV_VL4))

/*
** ptrue_pat_vl5_b32:
**	ptrue	p0\.s, vl5
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_vl5_b32,
		p0 = svptrue_pat_b32 (SV_VL5),
		p0 = svptrue_pat_b32 (SV_VL5))

/*
** ptrue_pat_vl6_b32:
**	ptrue	p0\.s, vl6
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_vl6_b32,
		p0 = svptrue_pat_b32 (SV_VL6),
		p0 = svptrue_pat_b32 (SV_VL6))

/*
** ptrue_pat_vl7_b32:
**	ptrue	p0\.s, vl7
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_vl7_b32,
		p0 = svptrue_pat_b32 (SV_VL7),
		p0 = svptrue_pat_b32 (SV_VL7))

/*
** ptrue_pat_vl8_b32:
**	ptrue	p0\.s, vl8
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_vl8_b32,
		p0 = svptrue_pat_b32 (SV_VL8),
		p0 = svptrue_pat_b32 (SV_VL8))

/*
** ptrue_pat_vl16_b32:
**	ptrue	p0\.[bhsd], vl16
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_vl16_b32,
		p0 = svptrue_pat_b32 (SV_VL16),
		p0 = svptrue_pat_b32 (SV_VL16))

/*
** ptrue_pat_vl32_b32:
**	ptrue	p0\.s, vl32
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_vl32_b32,
		p0 = svptrue_pat_b32 (SV_VL32),
		p0 = svptrue_pat_b32 (SV_VL32))

/*
** ptrue_pat_vl64_b32:
**	ptrue	p0\.s, vl64
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_vl64_b32,
		p0 = svptrue_pat_b32 (SV_VL64),
		p0 = svptrue_pat_b32 (SV_VL64))

/*
** ptrue_pat_vl128_b32:
**	ptrue	p0\.[bhsd], vl128
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_vl128_b32,
		p0 = svptrue_pat_b32 (SV_VL128),
		p0 = svptrue_pat_b32 (SV_VL128))

/*
** ptrue_pat_vl256_b32:
**	ptrue	p0\.s, vl256
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_vl256_b32,
		p0 = svptrue_pat_b32 (SV_VL256),
		p0 = svptrue_pat_b32 (SV_VL256))

/*
** ptrue_pat_mul4_b32:
**	ptrue	p0\.s, mul4
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_mul4_b32,
		p0 = svptrue_pat_b32 (SV_MUL4),
		p0 = svptrue_pat_b32 (SV_MUL4))

/*
** ptrue_pat_mul3_b32:
**	ptrue	p0\.s, mul3
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_mul3_b32,
		p0 = svptrue_pat_b32 (SV_MUL3),
		p0 = svptrue_pat_b32 (SV_MUL3))

/*
** ptrue_pat_all_b32:
**	ptrue	p0\.s[^\n]*
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_all_b32,
		p0 = svptrue_pat_b32 (SV_ALL),
		p0 = svptrue_pat_b32 (SV_ALL))
