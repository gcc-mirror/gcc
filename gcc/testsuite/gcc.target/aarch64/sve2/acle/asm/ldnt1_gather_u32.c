/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ldnt1_gather_u32_tied1:
**	ldnt1w	z0\.s, p0/z, \[z0\.s\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_u32_tied1, svuint32_t, svuint32_t,
		     z0_res = svldnt1_gather_u32base_u32 (p0, z0),
		     z0_res = svldnt1_gather_u32 (p0, z0))

/*
** ldnt1_gather_u32_untied:
**	ldnt1w	z0\.s, p0/z, \[z1\.s\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_u32_untied, svuint32_t, svuint32_t,
		     z0_res = svldnt1_gather_u32base_u32 (p0, z1),
		     z0_res = svldnt1_gather_u32 (p0, z1))

/*
** ldnt1_gather_x0_u32_offset:
**	ldnt1w	z0\.s, p0/z, \[z0\.s, x0\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_x0_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svldnt1_gather_u32base_offset_u32 (p0, z0, x0),
		     z0_res = svldnt1_gather_offset_u32 (p0, z0, x0))

/*
** ldnt1_gather_m4_u32_offset:
**	mov	(x[0-9]+), #?-4
**	ldnt1w	z0\.s, p0/z, \[z0\.s, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_m4_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svldnt1_gather_u32base_offset_u32 (p0, z0, -4),
		     z0_res = svldnt1_gather_offset_u32 (p0, z0, -4))

/*
** ldnt1_gather_0_u32_offset:
**	ldnt1w	z0\.s, p0/z, \[z0\.s\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_0_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svldnt1_gather_u32base_offset_u32 (p0, z0, 0),
		     z0_res = svldnt1_gather_offset_u32 (p0, z0, 0))

/*
** ldnt1_gather_5_u32_offset:
**	mov	(x[0-9]+), #?5
**	ldnt1w	z0\.s, p0/z, \[z0\.s, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_5_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svldnt1_gather_u32base_offset_u32 (p0, z0, 5),
		     z0_res = svldnt1_gather_offset_u32 (p0, z0, 5))

/*
** ldnt1_gather_6_u32_offset:
**	mov	(x[0-9]+), #?6
**	ldnt1w	z0\.s, p0/z, \[z0\.s, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_6_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svldnt1_gather_u32base_offset_u32 (p0, z0, 6),
		     z0_res = svldnt1_gather_offset_u32 (p0, z0, 6))

/*
** ldnt1_gather_7_u32_offset:
**	mov	(x[0-9]+), #?7
**	ldnt1w	z0\.s, p0/z, \[z0\.s, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_7_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svldnt1_gather_u32base_offset_u32 (p0, z0, 7),
		     z0_res = svldnt1_gather_offset_u32 (p0, z0, 7))

/*
** ldnt1_gather_8_u32_offset:
**	mov	(x[0-9]+), #?8
**	ldnt1w	z0\.s, p0/z, \[z0\.s, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_8_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svldnt1_gather_u32base_offset_u32 (p0, z0, 8),
		     z0_res = svldnt1_gather_offset_u32 (p0, z0, 8))

/*
** ldnt1_gather_124_u32_offset:
**	mov	(x[0-9]+), #?124
**	ldnt1w	z0\.s, p0/z, \[z0\.s, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_124_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svldnt1_gather_u32base_offset_u32 (p0, z0, 124),
		     z0_res = svldnt1_gather_offset_u32 (p0, z0, 124))

/*
** ldnt1_gather_128_u32_offset:
**	mov	(x[0-9]+), #?128
**	ldnt1w	z0\.s, p0/z, \[z0\.s, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_128_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svldnt1_gather_u32base_offset_u32 (p0, z0, 128),
		     z0_res = svldnt1_gather_offset_u32 (p0, z0, 128))

/*
** ldnt1_gather_x0_u32_index:
**	lsl	(x[0-9]+), x0, #?2
**	ldnt1w	z0\.s, p0/z, \[z0\.s, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_x0_u32_index, svuint32_t, svuint32_t,
		     z0_res = svldnt1_gather_u32base_index_u32 (p0, z0, x0),
		     z0_res = svldnt1_gather_index_u32 (p0, z0, x0))

/*
** ldnt1_gather_m1_u32_index:
**	mov	(x[0-9]+), #?-4
**	ldnt1w	z0\.s, p0/z, \[z0\.s, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_m1_u32_index, svuint32_t, svuint32_t,
		     z0_res = svldnt1_gather_u32base_index_u32 (p0, z0, -1),
		     z0_res = svldnt1_gather_index_u32 (p0, z0, -1))

/*
** ldnt1_gather_0_u32_index:
**	ldnt1w	z0\.s, p0/z, \[z0\.s\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_0_u32_index, svuint32_t, svuint32_t,
		     z0_res = svldnt1_gather_u32base_index_u32 (p0, z0, 0),
		     z0_res = svldnt1_gather_index_u32 (p0, z0, 0))

/*
** ldnt1_gather_5_u32_index:
**	mov	(x[0-9]+), #?20
**	ldnt1w	z0\.s, p0/z, \[z0\.s, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_5_u32_index, svuint32_t, svuint32_t,
		     z0_res = svldnt1_gather_u32base_index_u32 (p0, z0, 5),
		     z0_res = svldnt1_gather_index_u32 (p0, z0, 5))

/*
** ldnt1_gather_31_u32_index:
**	mov	(x[0-9]+), #?124
**	ldnt1w	z0\.s, p0/z, \[z0\.s, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_31_u32_index, svuint32_t, svuint32_t,
		     z0_res = svldnt1_gather_u32base_index_u32 (p0, z0, 31),
		     z0_res = svldnt1_gather_index_u32 (p0, z0, 31))

/*
** ldnt1_gather_32_u32_index:
**	mov	(x[0-9]+), #?128
**	ldnt1w	z0\.s, p0/z, \[z0\.s, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_32_u32_index, svuint32_t, svuint32_t,
		     z0_res = svldnt1_gather_u32base_index_u32 (p0, z0, 32),
		     z0_res = svldnt1_gather_index_u32 (p0, z0, 32))

/*
** ldnt1_gather_x0_u32_u32offset:
**	ldnt1w	z0\.s, p0/z, \[z0\.s, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1_gather_x0_u32_u32offset, svuint32_t, uint32_t, svuint32_t,
		     z0_res = svldnt1_gather_u32offset_u32 (p0, x0, z0),
		     z0_res = svldnt1_gather_offset (p0, x0, z0))

/*
** ldnt1_gather_tied1_u32_u32offset:
**	ldnt1w	z0\.s, p0/z, \[z0\.s, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1_gather_tied1_u32_u32offset, svuint32_t, uint32_t, svuint32_t,
		     z0_res = svldnt1_gather_u32offset_u32 (p0, x0, z0),
		     z0_res = svldnt1_gather_offset (p0, x0, z0))

/*
** ldnt1_gather_untied_u32_u32offset:
**	ldnt1w	z0\.s, p0/z, \[z1\.s, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1_gather_untied_u32_u32offset, svuint32_t, uint32_t, svuint32_t,
		     z0_res = svldnt1_gather_u32offset_u32 (p0, x0, z1),
		     z0_res = svldnt1_gather_offset (p0, x0, z1))
