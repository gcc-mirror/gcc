/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** adrb_u32base_s32offset:
**	adr	z0\.s, \[z0\.s, z1\.s\]
**	ret
*/
TEST_ADR (adrb_u32base_s32offset, svuint32_t, svint32_t,
	  z0 = svadrb_u32base_s32offset (z0, z1),
	  z0 = svadrb_offset (z0, z1))

/*
** adrb_u32base_u32offset:
**	adr	z0\.s, \[z0\.s, z1\.s\]
**	ret
*/
TEST_ADR (adrb_u32base_u32offset, svuint32_t, svuint32_t,
	  z0 = svadrb_u32base_u32offset (z0, z1),
	  z0 = svadrb_offset (z0, z1))

/*
** adrb_u64base_s64offset:
**	adr	z0\.d, \[z0\.d, z1\.d\]
**	ret
*/
TEST_ADR (adrb_u64base_s64offset, svuint64_t, svint64_t,
	  z0 = svadrb_u64base_s64offset (z0, z1),
	  z0 = svadrb_offset (z0, z1))

/*
** adrb_ext_u64base_s64offset:
**	adr	z0\.d, \[z0\.d, z1\.d, sxtw\]
**	ret
*/
TEST_ADR (adrb_ext_u64base_s64offset, svuint64_t, svint64_t,
	  z0 = svadrb_u64base_s64offset (z0, svextw_s64_x (svptrue_b64 (), z1)),
	  z0 = svadrb_offset (z0, svextw_x (svptrue_b64 (), z1)))

/*
** adrb_u64base_u64offset:
**	adr	z0\.d, \[z0\.d, z1\.d\]
**	ret
*/
TEST_ADR (adrb_u64base_u64offset, svuint64_t, svuint64_t,
	  z0 = svadrb_u64base_u64offset (z0, z1),
	  z0 = svadrb_offset (z0, z1))

/*
** adrb_ext_u64base_u64offset:
**	adr	z0\.d, \[z0\.d, z1\.d, uxtw\]
**	ret
*/
TEST_ADR (adrb_ext_u64base_u64offset, svuint64_t, svuint64_t,
	  z0 = svadrb_u64base_u64offset (z0, svextw_u64_x (svptrue_b64 (), z1)),
	  z0 = svadrb_offset (z0, svextw_x (svptrue_b64 (), z1)))
