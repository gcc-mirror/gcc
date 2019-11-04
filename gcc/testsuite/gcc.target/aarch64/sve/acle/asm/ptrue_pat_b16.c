/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** ptrue_pat_pow2_b16:
**	ptrue	p0\.h, pow2
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_pow2_b16,
		p0 = svptrue_pat_b16 (SV_POW2),
		p0 = svptrue_pat_b16 (SV_POW2))

/*
** ptrue_pat_vl1_b16:
**	ptrue	p0\.[bhsd], vl1
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_vl1_b16,
		p0 = svptrue_pat_b16 (SV_VL1),
		p0 = svptrue_pat_b16 (SV_VL1))

/*
** ptrue_pat_vl2_b16:
**	ptrue	p0\.h, vl2
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_vl2_b16,
		p0 = svptrue_pat_b16 (SV_VL2),
		p0 = svptrue_pat_b16 (SV_VL2))

/*
** ptrue_pat_vl3_b16:
**	ptrue	p0\.h, vl3
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_vl3_b16,
		p0 = svptrue_pat_b16 (SV_VL3),
		p0 = svptrue_pat_b16 (SV_VL3))

/*
** ptrue_pat_vl4_b16:
**	ptrue	p0\.h, vl4
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_vl4_b16,
		p0 = svptrue_pat_b16 (SV_VL4),
		p0 = svptrue_pat_b16 (SV_VL4))

/*
** ptrue_pat_vl5_b16:
**	ptrue	p0\.h, vl5
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_vl5_b16,
		p0 = svptrue_pat_b16 (SV_VL5),
		p0 = svptrue_pat_b16 (SV_VL5))

/*
** ptrue_pat_vl6_b16:
**	ptrue	p0\.h, vl6
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_vl6_b16,
		p0 = svptrue_pat_b16 (SV_VL6),
		p0 = svptrue_pat_b16 (SV_VL6))

/*
** ptrue_pat_vl7_b16:
**	ptrue	p0\.h, vl7
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_vl7_b16,
		p0 = svptrue_pat_b16 (SV_VL7),
		p0 = svptrue_pat_b16 (SV_VL7))

/*
** ptrue_pat_vl8_b16:
**	ptrue	p0\.h, vl8
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_vl8_b16,
		p0 = svptrue_pat_b16 (SV_VL8),
		p0 = svptrue_pat_b16 (SV_VL8))

/*
** ptrue_pat_vl16_b16:
**	ptrue	p0\.[bhsd], vl16
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_vl16_b16,
		p0 = svptrue_pat_b16 (SV_VL16),
		p0 = svptrue_pat_b16 (SV_VL16))

/*
** ptrue_pat_vl32_b16:
**	ptrue	p0\.h, vl32
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_vl32_b16,
		p0 = svptrue_pat_b16 (SV_VL32),
		p0 = svptrue_pat_b16 (SV_VL32))

/*
** ptrue_pat_vl64_b16:
**	ptrue	p0\.h, vl64
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_vl64_b16,
		p0 = svptrue_pat_b16 (SV_VL64),
		p0 = svptrue_pat_b16 (SV_VL64))

/*
** ptrue_pat_vl128_b16:
**	ptrue	p0\.[bhsd], vl128
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_vl128_b16,
		p0 = svptrue_pat_b16 (SV_VL128),
		p0 = svptrue_pat_b16 (SV_VL128))

/*
** ptrue_pat_vl256_b16:
**	ptrue	p0\.h, vl256
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_vl256_b16,
		p0 = svptrue_pat_b16 (SV_VL256),
		p0 = svptrue_pat_b16 (SV_VL256))

/*
** ptrue_pat_mul4_b16:
**	ptrue	p0\.h, mul4
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_mul4_b16,
		p0 = svptrue_pat_b16 (SV_MUL4),
		p0 = svptrue_pat_b16 (SV_MUL4))

/*
** ptrue_pat_mul3_b16:
**	ptrue	p0\.h, mul3
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_mul3_b16,
		p0 = svptrue_pat_b16 (SV_MUL3),
		p0 = svptrue_pat_b16 (SV_MUL3))

/*
** ptrue_pat_all_b16:
**	ptrue	p0\.h[^\n]*
**	ret
*/
TEST_UNIFORM_P (ptrue_pat_all_b16,
		p0 = svptrue_pat_b16 (SV_ALL),
		p0 = svptrue_pat_b16 (SV_ALL))
