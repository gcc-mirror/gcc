/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ldnt1uh_gather_s64_tied1:
**	ldnt1h	z0\.d, p0/z, \[z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1uh_gather_s64_tied1, svint64_t, svuint64_t,
		     z0_res = svldnt1uh_gather_u64base_s64 (p0, z0),
		     z0_res = svldnt1uh_gather_s64 (p0, z0))

/*
** ldnt1uh_gather_s64_untied:
**	ldnt1h	z0\.d, p0/z, \[z1\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1uh_gather_s64_untied, svint64_t, svuint64_t,
		     z0_res = svldnt1uh_gather_u64base_s64 (p0, z1),
		     z0_res = svldnt1uh_gather_s64 (p0, z1))

/*
** ldnt1uh_gather_x0_s64_offset:
**	ldnt1h	z0\.d, p0/z, \[z0\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1uh_gather_x0_s64_offset, svint64_t, svuint64_t,
		     z0_res = svldnt1uh_gather_u64base_offset_s64 (p0, z0, x0),
		     z0_res = svldnt1uh_gather_offset_s64 (p0, z0, x0))

/*
** ldnt1uh_gather_m2_s64_offset:
**	mov	(x[0-9]+), #?-2
**	ldnt1h	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1uh_gather_m2_s64_offset, svint64_t, svuint64_t,
		     z0_res = svldnt1uh_gather_u64base_offset_s64 (p0, z0, -2),
		     z0_res = svldnt1uh_gather_offset_s64 (p0, z0, -2))

/*
** ldnt1uh_gather_0_s64_offset:
**	ldnt1h	z0\.d, p0/z, \[z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1uh_gather_0_s64_offset, svint64_t, svuint64_t,
		     z0_res = svldnt1uh_gather_u64base_offset_s64 (p0, z0, 0),
		     z0_res = svldnt1uh_gather_offset_s64 (p0, z0, 0))

/*
** ldnt1uh_gather_5_s64_offset:
**	mov	(x[0-9]+), #?5
**	ldnt1h	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1uh_gather_5_s64_offset, svint64_t, svuint64_t,
		     z0_res = svldnt1uh_gather_u64base_offset_s64 (p0, z0, 5),
		     z0_res = svldnt1uh_gather_offset_s64 (p0, z0, 5))

/*
** ldnt1uh_gather_6_s64_offset:
**	mov	(x[0-9]+), #?6
**	ldnt1h	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1uh_gather_6_s64_offset, svint64_t, svuint64_t,
		     z0_res = svldnt1uh_gather_u64base_offset_s64 (p0, z0, 6),
		     z0_res = svldnt1uh_gather_offset_s64 (p0, z0, 6))

/*
** ldnt1uh_gather_62_s64_offset:
**	mov	(x[0-9]+), #?62
**	ldnt1h	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1uh_gather_62_s64_offset, svint64_t, svuint64_t,
		     z0_res = svldnt1uh_gather_u64base_offset_s64 (p0, z0, 62),
		     z0_res = svldnt1uh_gather_offset_s64 (p0, z0, 62))

/*
** ldnt1uh_gather_64_s64_offset:
**	mov	(x[0-9]+), #?64
**	ldnt1h	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1uh_gather_64_s64_offset, svint64_t, svuint64_t,
		     z0_res = svldnt1uh_gather_u64base_offset_s64 (p0, z0, 64),
		     z0_res = svldnt1uh_gather_offset_s64 (p0, z0, 64))

/*
** ldnt1uh_gather_x0_s64_index:
**	lsl	(x[0-9]+), x0, #?1
**	ldnt1h	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1uh_gather_x0_s64_index, svint64_t, svuint64_t,
		     z0_res = svldnt1uh_gather_u64base_index_s64 (p0, z0, x0),
		     z0_res = svldnt1uh_gather_index_s64 (p0, z0, x0))

/*
** ldnt1uh_gather_m1_s64_index:
**	mov	(x[0-9]+), #?-2
**	ldnt1h	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1uh_gather_m1_s64_index, svint64_t, svuint64_t,
		     z0_res = svldnt1uh_gather_u64base_index_s64 (p0, z0, -1),
		     z0_res = svldnt1uh_gather_index_s64 (p0, z0, -1))

/*
** ldnt1uh_gather_0_s64_index:
**	ldnt1h	z0\.d, p0/z, \[z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1uh_gather_0_s64_index, svint64_t, svuint64_t,
		     z0_res = svldnt1uh_gather_u64base_index_s64 (p0, z0, 0),
		     z0_res = svldnt1uh_gather_index_s64 (p0, z0, 0))

/*
** ldnt1uh_gather_5_s64_index:
**	mov	(x[0-9]+), #?10
**	ldnt1h	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1uh_gather_5_s64_index, svint64_t, svuint64_t,
		     z0_res = svldnt1uh_gather_u64base_index_s64 (p0, z0, 5),
		     z0_res = svldnt1uh_gather_index_s64 (p0, z0, 5))

/*
** ldnt1uh_gather_31_s64_index:
**	mov	(x[0-9]+), #?62
**	ldnt1h	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1uh_gather_31_s64_index, svint64_t, svuint64_t,
		     z0_res = svldnt1uh_gather_u64base_index_s64 (p0, z0, 31),
		     z0_res = svldnt1uh_gather_index_s64 (p0, z0, 31))

/*
** ldnt1uh_gather_32_s64_index:
**	mov	(x[0-9]+), #?64
**	ldnt1h	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1uh_gather_32_s64_index, svint64_t, svuint64_t,
		     z0_res = svldnt1uh_gather_u64base_index_s64 (p0, z0, 32),
		     z0_res = svldnt1uh_gather_index_s64 (p0, z0, 32))

/*
** ldnt1uh_gather_x0_s64_s64offset:
**	ldnt1h	z0\.d, p0/z, \[z0\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1uh_gather_x0_s64_s64offset, svint64_t, uint16_t, svint64_t,
		     z0_res = svldnt1uh_gather_s64offset_s64 (p0, x0, z0),
		     z0_res = svldnt1uh_gather_offset_s64 (p0, x0, z0))

/*
** ldnt1uh_gather_tied1_s64_s64offset:
**	ldnt1h	z0\.d, p0/z, \[z0\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1uh_gather_tied1_s64_s64offset, svint64_t, uint16_t, svint64_t,
		     z0_res = svldnt1uh_gather_s64offset_s64 (p0, x0, z0),
		     z0_res = svldnt1uh_gather_offset_s64 (p0, x0, z0))

/*
** ldnt1uh_gather_untied_s64_s64offset:
**	ldnt1h	z0\.d, p0/z, \[z1\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1uh_gather_untied_s64_s64offset, svint64_t, uint16_t, svint64_t,
		     z0_res = svldnt1uh_gather_s64offset_s64 (p0, x0, z1),
		     z0_res = svldnt1uh_gather_offset_s64 (p0, x0, z1))

/*
** ldnt1uh_gather_x0_s64_u64offset:
**	ldnt1h	z0\.d, p0/z, \[z0\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1uh_gather_x0_s64_u64offset, svint64_t, uint16_t, svuint64_t,
		     z0_res = svldnt1uh_gather_u64offset_s64 (p0, x0, z0),
		     z0_res = svldnt1uh_gather_offset_s64 (p0, x0, z0))

/*
** ldnt1uh_gather_tied1_s64_u64offset:
**	ldnt1h	z0\.d, p0/z, \[z0\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1uh_gather_tied1_s64_u64offset, svint64_t, uint16_t, svuint64_t,
		     z0_res = svldnt1uh_gather_u64offset_s64 (p0, x0, z0),
		     z0_res = svldnt1uh_gather_offset_s64 (p0, x0, z0))

/*
** ldnt1uh_gather_untied_s64_u64offset:
**	ldnt1h	z0\.d, p0/z, \[z1\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1uh_gather_untied_s64_u64offset, svint64_t, uint16_t, svuint64_t,
		     z0_res = svldnt1uh_gather_u64offset_s64 (p0, x0, z1),
		     z0_res = svldnt1uh_gather_offset_s64 (p0, x0, z1))

/*
** ldnt1uh_gather_x0_s64_s64index:
**	lsl	(z[0-9]+\.d), z0\.d, #1
**	ldnt1h	z0\.d, p0/z, \[\1, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1uh_gather_x0_s64_s64index, svint64_t, uint16_t, svint64_t,
		     z0_res = svldnt1uh_gather_s64index_s64 (p0, x0, z0),
		     z0_res = svldnt1uh_gather_index_s64 (p0, x0, z0))

/*
** ldnt1uh_gather_tied1_s64_s64index:
**	lsl	(z[0-9]+\.d), z0\.d, #1
**	ldnt1h	z0\.d, p0/z, \[\1, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1uh_gather_tied1_s64_s64index, svint64_t, uint16_t, svint64_t,
		     z0_res = svldnt1uh_gather_s64index_s64 (p0, x0, z0),
		     z0_res = svldnt1uh_gather_index_s64 (p0, x0, z0))

/*
** ldnt1uh_gather_untied_s64_s64index:
**	lsl	(z[0-9]+\.d), z1\.d, #1
**	ldnt1h	z0\.d, p0/z, \[\1, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1uh_gather_untied_s64_s64index, svint64_t, uint16_t, svint64_t,
		     z0_res = svldnt1uh_gather_s64index_s64 (p0, x0, z1),
		     z0_res = svldnt1uh_gather_index_s64 (p0, x0, z1))

/*
** ldnt1uh_gather_x0_s64_u64index:
**	lsl	(z[0-9]+\.d), z0\.d, #1
**	ldnt1h	z0\.d, p0/z, \[\1, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1uh_gather_x0_s64_u64index, svint64_t, uint16_t, svuint64_t,
		     z0_res = svldnt1uh_gather_u64index_s64 (p0, x0, z0),
		     z0_res = svldnt1uh_gather_index_s64 (p0, x0, z0))

/*
** ldnt1uh_gather_tied1_s64_u64index:
**	lsl	(z[0-9]+\.d), z0\.d, #1
**	ldnt1h	z0\.d, p0/z, \[\1, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1uh_gather_tied1_s64_u64index, svint64_t, uint16_t, svuint64_t,
		     z0_res = svldnt1uh_gather_u64index_s64 (p0, x0, z0),
		     z0_res = svldnt1uh_gather_index_s64 (p0, x0, z0))

/*
** ldnt1uh_gather_untied_s64_u64index:
**	lsl	(z[0-9]+\.d), z1\.d, #1
**	ldnt1h	z0\.d, p0/z, \[\1, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1uh_gather_untied_s64_u64index, svint64_t, uint16_t, svuint64_t,
		     z0_res = svldnt1uh_gather_u64index_s64 (p0, x0, z1),
		     z0_res = svldnt1uh_gather_index_s64 (p0, x0, z1))
