/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** adrh_u32base_s32index:
**	adr	z0\.s, \[z0\.s, z1\.s, lsl 1\]
**	ret
*/
TEST_ADR (adrh_u32base_s32index, svuint32_t, svint32_t,
	  z0 = svadrh_u32base_s32index (z0, z1),
	  z0 = svadrh_index (z0, z1))

/*
** adrh_u32base_u32index:
**	adr	z0\.s, \[z0\.s, z1\.s, lsl 1\]
**	ret
*/
TEST_ADR (adrh_u32base_u32index, svuint32_t, svuint32_t,
	  z0 = svadrh_u32base_u32index (z0, z1),
	  z0 = svadrh_index (z0, z1))

/*
** adrh_u64base_s64index:
**	adr	z0\.d, \[z0\.d, z1\.d, lsl 1\]
**	ret
*/
TEST_ADR (adrh_u64base_s64index, svuint64_t, svint64_t,
	  z0 = svadrh_u64base_s64index (z0, z1),
	  z0 = svadrh_index (z0, z1))

/*
** adrh_ext_u64base_s64index:
**	adr	z0\.d, \[z0\.d, z1\.d, sxtw 1\]
**	ret
*/
TEST_ADR (adrh_ext_u64base_s64index, svuint64_t, svint64_t,
	  z0 = svadrh_u64base_s64index (z0, svextw_s64_x (svptrue_b64 (), z1)),
	  z0 = svadrh_index (z0, svextw_x (svptrue_b64 (), z1)))

/*
** adrh_u64base_u64index:
**	adr	z0\.d, \[z0\.d, z1\.d, lsl 1\]
**	ret
*/
TEST_ADR (adrh_u64base_u64index, svuint64_t, svuint64_t,
	  z0 = svadrh_u64base_u64index (z0, z1),
	  z0 = svadrh_index (z0, z1))

/*
** adrh_ext_u64base_u64index:
**	adr	z0\.d, \[z0\.d, z1\.d, uxtw 1\]
**	ret
*/
TEST_ADR (adrh_ext_u64base_u64index, svuint64_t, svuint64_t,
	  z0 = svadrh_u64base_u64index (z0, svextw_u64_x (svptrue_b64 (), z1)),
	  z0 = svadrh_index (z0, svextw_x (svptrue_b64 (), z1)))
