/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ldnt1sb_gather_s64_tied1:
**	ldnt1sb	z0\.d, p0/z, \[z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1sb_gather_s64_tied1, svint64_t, svuint64_t,
		     z0_res = svldnt1sb_gather_u64base_s64 (p0, z0),
		     z0_res = svldnt1sb_gather_s64 (p0, z0))

/*
** ldnt1sb_gather_s64_untied:
**	ldnt1sb	z0\.d, p0/z, \[z1\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1sb_gather_s64_untied, svint64_t, svuint64_t,
		     z0_res = svldnt1sb_gather_u64base_s64 (p0, z1),
		     z0_res = svldnt1sb_gather_s64 (p0, z1))

/*
** ldnt1sb_gather_x0_s64_offset:
**	ldnt1sb	z0\.d, p0/z, \[z0\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1sb_gather_x0_s64_offset, svint64_t, svuint64_t,
		     z0_res = svldnt1sb_gather_u64base_offset_s64 (p0, z0, x0),
		     z0_res = svldnt1sb_gather_offset_s64 (p0, z0, x0))

/*
** ldnt1sb_gather_m1_s64_offset:
**	mov	(x[0-9]+), #?-1
**	ldnt1sb	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1sb_gather_m1_s64_offset, svint64_t, svuint64_t,
		     z0_res = svldnt1sb_gather_u64base_offset_s64 (p0, z0, -1),
		     z0_res = svldnt1sb_gather_offset_s64 (p0, z0, -1))

/*
** ldnt1sb_gather_0_s64_offset:
**	ldnt1sb	z0\.d, p0/z, \[z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1sb_gather_0_s64_offset, svint64_t, svuint64_t,
		     z0_res = svldnt1sb_gather_u64base_offset_s64 (p0, z0, 0),
		     z0_res = svldnt1sb_gather_offset_s64 (p0, z0, 0))

/*
** ldnt1sb_gather_5_s64_offset:
**	mov	(x[0-9]+), #?5
**	ldnt1sb	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1sb_gather_5_s64_offset, svint64_t, svuint64_t,
		     z0_res = svldnt1sb_gather_u64base_offset_s64 (p0, z0, 5),
		     z0_res = svldnt1sb_gather_offset_s64 (p0, z0, 5))

/*
** ldnt1sb_gather_31_s64_offset:
**	mov	(x[0-9]+), #?31
**	ldnt1sb	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1sb_gather_31_s64_offset, svint64_t, svuint64_t,
		     z0_res = svldnt1sb_gather_u64base_offset_s64 (p0, z0, 31),
		     z0_res = svldnt1sb_gather_offset_s64 (p0, z0, 31))

/*
** ldnt1sb_gather_32_s64_offset:
**	mov	(x[0-9]+), #?32
**	ldnt1sb	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1sb_gather_32_s64_offset, svint64_t, svuint64_t,
		     z0_res = svldnt1sb_gather_u64base_offset_s64 (p0, z0, 32),
		     z0_res = svldnt1sb_gather_offset_s64 (p0, z0, 32))

/*
** ldnt1sb_gather_x0_s64_s64offset:
**	ldnt1sb	z0\.d, p0/z, \[z0\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1sb_gather_x0_s64_s64offset, svint64_t, int8_t, svint64_t,
		     z0_res = svldnt1sb_gather_s64offset_s64 (p0, x0, z0),
		     z0_res = svldnt1sb_gather_offset_s64 (p0, x0, z0))

/*
** ldnt1sb_gather_tied1_s64_s64offset:
**	ldnt1sb	z0\.d, p0/z, \[z0\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1sb_gather_tied1_s64_s64offset, svint64_t, int8_t, svint64_t,
		     z0_res = svldnt1sb_gather_s64offset_s64 (p0, x0, z0),
		     z0_res = svldnt1sb_gather_offset_s64 (p0, x0, z0))

/*
** ldnt1sb_gather_untied_s64_s64offset:
**	ldnt1sb	z0\.d, p0/z, \[z1\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1sb_gather_untied_s64_s64offset, svint64_t, int8_t, svint64_t,
		     z0_res = svldnt1sb_gather_s64offset_s64 (p0, x0, z1),
		     z0_res = svldnt1sb_gather_offset_s64 (p0, x0, z1))

/*
** ldnt1sb_gather_x0_s64_u64offset:
**	ldnt1sb	z0\.d, p0/z, \[z0\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1sb_gather_x0_s64_u64offset, svint64_t, int8_t, svuint64_t,
		     z0_res = svldnt1sb_gather_u64offset_s64 (p0, x0, z0),
		     z0_res = svldnt1sb_gather_offset_s64 (p0, x0, z0))

/*
** ldnt1sb_gather_tied1_s64_u64offset:
**	ldnt1sb	z0\.d, p0/z, \[z0\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1sb_gather_tied1_s64_u64offset, svint64_t, int8_t, svuint64_t,
		     z0_res = svldnt1sb_gather_u64offset_s64 (p0, x0, z0),
		     z0_res = svldnt1sb_gather_offset_s64 (p0, x0, z0))

/*
** ldnt1sb_gather_untied_s64_u64offset:
**	ldnt1sb	z0\.d, p0/z, \[z1\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1sb_gather_untied_s64_u64offset, svint64_t, int8_t, svuint64_t,
		     z0_res = svldnt1sb_gather_u64offset_s64 (p0, x0, z1),
		     z0_res = svldnt1sb_gather_offset_s64 (p0, x0, z1))
