/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ldnt1sb_gather_u32_tied1:
**	ldnt1sb	z0\.s, p0/z, \[z0\.s\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1sb_gather_u32_tied1, svuint32_t, svuint32_t,
		     z0_res = svldnt1sb_gather_u32base_u32 (p0, z0),
		     z0_res = svldnt1sb_gather_u32 (p0, z0))

/*
** ldnt1sb_gather_u32_untied:
**	ldnt1sb	z0\.s, p0/z, \[z1\.s\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1sb_gather_u32_untied, svuint32_t, svuint32_t,
		     z0_res = svldnt1sb_gather_u32base_u32 (p0, z1),
		     z0_res = svldnt1sb_gather_u32 (p0, z1))

/*
** ldnt1sb_gather_x0_u32_offset:
**	ldnt1sb	z0\.s, p0/z, \[z0\.s, x0\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1sb_gather_x0_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svldnt1sb_gather_u32base_offset_u32 (p0, z0, x0),
		     z0_res = svldnt1sb_gather_offset_u32 (p0, z0, x0))

/*
** ldnt1sb_gather_m1_u32_offset:
**	mov	(x[0-9]+), #?-1
**	ldnt1sb	z0\.s, p0/z, \[z0\.s, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1sb_gather_m1_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svldnt1sb_gather_u32base_offset_u32 (p0, z0, -1),
		     z0_res = svldnt1sb_gather_offset_u32 (p0, z0, -1))

/*
** ldnt1sb_gather_0_u32_offset:
**	ldnt1sb	z0\.s, p0/z, \[z0\.s\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1sb_gather_0_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svldnt1sb_gather_u32base_offset_u32 (p0, z0, 0),
		     z0_res = svldnt1sb_gather_offset_u32 (p0, z0, 0))

/*
** ldnt1sb_gather_5_u32_offset:
**	mov	(x[0-9]+), #?5
**	ldnt1sb	z0\.s, p0/z, \[z0\.s, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1sb_gather_5_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svldnt1sb_gather_u32base_offset_u32 (p0, z0, 5),
		     z0_res = svldnt1sb_gather_offset_u32 (p0, z0, 5))

/*
** ldnt1sb_gather_31_u32_offset:
**	mov	(x[0-9]+), #?31
**	ldnt1sb	z0\.s, p0/z, \[z0\.s, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1sb_gather_31_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svldnt1sb_gather_u32base_offset_u32 (p0, z0, 31),
		     z0_res = svldnt1sb_gather_offset_u32 (p0, z0, 31))

/*
** ldnt1sb_gather_32_u32_offset:
**	mov	(x[0-9]+), #?32
**	ldnt1sb	z0\.s, p0/z, \[z0\.s, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1sb_gather_32_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svldnt1sb_gather_u32base_offset_u32 (p0, z0, 32),
		     z0_res = svldnt1sb_gather_offset_u32 (p0, z0, 32))

/*
** ldnt1sb_gather_x0_u32_u32offset:
**	ldnt1sb	z0\.s, p0/z, \[z0\.s, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1sb_gather_x0_u32_u32offset, svuint32_t, int8_t, svuint32_t,
		     z0_res = svldnt1sb_gather_u32offset_u32 (p0, x0, z0),
		     z0_res = svldnt1sb_gather_offset_u32 (p0, x0, z0))

/*
** ldnt1sb_gather_tied1_u32_u32offset:
**	ldnt1sb	z0\.s, p0/z, \[z0\.s, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1sb_gather_tied1_u32_u32offset, svuint32_t, int8_t, svuint32_t,
		     z0_res = svldnt1sb_gather_u32offset_u32 (p0, x0, z0),
		     z0_res = svldnt1sb_gather_offset_u32 (p0, x0, z0))

/*
** ldnt1sb_gather_untied_u32_u32offset:
**	ldnt1sb	z0\.s, p0/z, \[z1\.s, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1sb_gather_untied_u32_u32offset, svuint32_t, int8_t, svuint32_t,
		     z0_res = svldnt1sb_gather_u32offset_u32 (p0, x0, z1),
		     z0_res = svldnt1sb_gather_offset_u32 (p0, x0, z1))
