/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ldnt1ub_gather_u64_tied1:
**	ldnt1b	z0\.d, p0/z, \[z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1ub_gather_u64_tied1, svuint64_t, svuint64_t,
		     z0_res = svldnt1ub_gather_u64base_u64 (p0, z0),
		     z0_res = svldnt1ub_gather_u64 (p0, z0))

/*
** ldnt1ub_gather_u64_untied:
**	ldnt1b	z0\.d, p0/z, \[z1\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1ub_gather_u64_untied, svuint64_t, svuint64_t,
		     z0_res = svldnt1ub_gather_u64base_u64 (p0, z1),
		     z0_res = svldnt1ub_gather_u64 (p0, z1))

/*
** ldnt1ub_gather_x0_u64_offset:
**	ldnt1b	z0\.d, p0/z, \[z0\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1ub_gather_x0_u64_offset, svuint64_t, svuint64_t,
		     z0_res = svldnt1ub_gather_u64base_offset_u64 (p0, z0, x0),
		     z0_res = svldnt1ub_gather_offset_u64 (p0, z0, x0))

/*
** ldnt1ub_gather_m1_u64_offset:
**	mov	(x[0-9]+), #?-1
**	ldnt1b	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1ub_gather_m1_u64_offset, svuint64_t, svuint64_t,
		     z0_res = svldnt1ub_gather_u64base_offset_u64 (p0, z0, -1),
		     z0_res = svldnt1ub_gather_offset_u64 (p0, z0, -1))

/*
** ldnt1ub_gather_0_u64_offset:
**	ldnt1b	z0\.d, p0/z, \[z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1ub_gather_0_u64_offset, svuint64_t, svuint64_t,
		     z0_res = svldnt1ub_gather_u64base_offset_u64 (p0, z0, 0),
		     z0_res = svldnt1ub_gather_offset_u64 (p0, z0, 0))

/*
** ldnt1ub_gather_5_u64_offset:
**	mov	(x[0-9]+), #?5
**	ldnt1b	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1ub_gather_5_u64_offset, svuint64_t, svuint64_t,
		     z0_res = svldnt1ub_gather_u64base_offset_u64 (p0, z0, 5),
		     z0_res = svldnt1ub_gather_offset_u64 (p0, z0, 5))

/*
** ldnt1ub_gather_31_u64_offset:
**	mov	(x[0-9]+), #?31
**	ldnt1b	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1ub_gather_31_u64_offset, svuint64_t, svuint64_t,
		     z0_res = svldnt1ub_gather_u64base_offset_u64 (p0, z0, 31),
		     z0_res = svldnt1ub_gather_offset_u64 (p0, z0, 31))

/*
** ldnt1ub_gather_32_u64_offset:
**	mov	(x[0-9]+), #?32
**	ldnt1b	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1ub_gather_32_u64_offset, svuint64_t, svuint64_t,
		     z0_res = svldnt1ub_gather_u64base_offset_u64 (p0, z0, 32),
		     z0_res = svldnt1ub_gather_offset_u64 (p0, z0, 32))

/*
** ldnt1ub_gather_x0_u64_s64offset:
**	ldnt1b	z0\.d, p0/z, \[z0\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1ub_gather_x0_u64_s64offset, svuint64_t, uint8_t, svint64_t,
		     z0_res = svldnt1ub_gather_s64offset_u64 (p0, x0, z0),
		     z0_res = svldnt1ub_gather_offset_u64 (p0, x0, z0))

/*
** ldnt1ub_gather_tied1_u64_s64offset:
**	ldnt1b	z0\.d, p0/z, \[z0\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1ub_gather_tied1_u64_s64offset, svuint64_t, uint8_t, svint64_t,
		     z0_res = svldnt1ub_gather_s64offset_u64 (p0, x0, z0),
		     z0_res = svldnt1ub_gather_offset_u64 (p0, x0, z0))

/*
** ldnt1ub_gather_untied_u64_s64offset:
**	ldnt1b	z0\.d, p0/z, \[z1\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1ub_gather_untied_u64_s64offset, svuint64_t, uint8_t, svint64_t,
		     z0_res = svldnt1ub_gather_s64offset_u64 (p0, x0, z1),
		     z0_res = svldnt1ub_gather_offset_u64 (p0, x0, z1))

/*
** ldnt1ub_gather_x0_u64_u64offset:
**	ldnt1b	z0\.d, p0/z, \[z0\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1ub_gather_x0_u64_u64offset, svuint64_t, uint8_t, svuint64_t,
		     z0_res = svldnt1ub_gather_u64offset_u64 (p0, x0, z0),
		     z0_res = svldnt1ub_gather_offset_u64 (p0, x0, z0))

/*
** ldnt1ub_gather_tied1_u64_u64offset:
**	ldnt1b	z0\.d, p0/z, \[z0\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1ub_gather_tied1_u64_u64offset, svuint64_t, uint8_t, svuint64_t,
		     z0_res = svldnt1ub_gather_u64offset_u64 (p0, x0, z0),
		     z0_res = svldnt1ub_gather_offset_u64 (p0, x0, z0))

/*
** ldnt1ub_gather_untied_u64_u64offset:
**	ldnt1b	z0\.d, p0/z, \[z1\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1ub_gather_untied_u64_u64offset, svuint64_t, uint8_t, svuint64_t,
		     z0_res = svldnt1ub_gather_u64offset_u64 (p0, x0, z1),
		     z0_res = svldnt1ub_gather_offset_u64 (p0, x0, z1))
