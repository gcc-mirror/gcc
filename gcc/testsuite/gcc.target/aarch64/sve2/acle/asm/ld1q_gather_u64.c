/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** ld1q_gather_tied:
**	ld1q	{z0\.q}, p0/z, \[z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1q_gather_tied, svuint64_t, svuint64_t,
		     z0_res = svld1q_gather_u64base_u64 (p0, z0),
		     z0_res = svld1q_gather_u64 (p0, z0))

/*
** ld1q_gather_untied:
**	ld1q	{z0\.q}, p0/z, \[z1\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1q_gather_untied, svuint64_t, svuint64_t,
		     z0_res = svld1q_gather_u64base_u64 (p0, z1),
		     z0_res = svld1q_gather_u64 (p0, z1))

/*
** ld1q_gather_x0_offset:
**	ld1q	{z0\.q}, p0/z, \[z0\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1q_gather_x0_offset, svuint64_t, svuint64_t,
		     z0_res = svld1q_gather_u64base_offset_u64 (p0, z0, x0),
		     z0_res = svld1q_gather_offset_u64 (p0, z0, x0))

/*
** ld1q_gather_m8_offset:
**	mov	(x[0-9]+), #?-8
**	ld1q	{z0\.q}, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1q_gather_m8_offset, svuint64_t, svuint64_t,
		     z0_res = svld1q_gather_u64base_offset_u64 (p0, z0, -8),
		     z0_res = svld1q_gather_offset_u64 (p0, z0, -8))

/*
** ld1q_gather_0_offset:
**	ld1q	{z0\.q}, p0/z, \[z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1q_gather_0_offset, svuint64_t, svuint64_t,
		     z0_res = svld1q_gather_u64base_offset_u64 (p0, z0, 0),
		     z0_res = svld1q_gather_offset_u64 (p0, z0, 0))

/*
** ld1q_gather_5_offset:
**	mov	(x[0-9]+), #?5
**	ld1q	{z0\.q}, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1q_gather_5_offset, svuint64_t, svuint64_t,
		     z0_res = svld1q_gather_u64base_offset_u64 (p0, z0, 5),
		     z0_res = svld1q_gather_offset_u64 (p0, z0, 5))

/*
** ld1q_gather_x0_index:
**	lsl	(x[0-9]+), x0, #?3
**	ld1q	{z0\.q}, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1q_gather_x0_index, svuint64_t, svuint64_t,
		     z0_res = svld1q_gather_u64base_index_u64 (p0, z0, x0),
		     z0_res = svld1q_gather_index_u64 (p0, z0, x0))

/*
** ld1q_gather_m1_index:
**	mov	(x[0-9]+), #?-8
**	ld1q	{z0\.q}, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1q_gather_m1_index, svuint64_t, svuint64_t,
		     z0_res = svld1q_gather_u64base_index_u64 (p0, z0, -1),
		     z0_res = svld1q_gather_index_u64 (p0, z0, -1))

/*
** ld1q_gather_0_index:
**	ld1q	{z0\.q}, p0/z, \[z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1q_gather_0_index, svuint64_t, svuint64_t,
		     z0_res = svld1q_gather_u64base_index_u64 (p0, z0, 0),
		     z0_res = svld1q_gather_index_u64 (p0, z0, 0))

/*
** ld1q_gather_5_index:
**	mov	(x[0-9]+), #?40
**	ld1q	{z0\.q}, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1q_gather_5_index, svuint64_t, svuint64_t,
		     z0_res = svld1q_gather_u64base_index_u64 (p0, z0, 5),
		     z0_res = svld1q_gather_index_u64 (p0, z0, 5))

/*
** ld1q_gather_s64offset_tied:
**	ld1q	{z0\.q}, p0/z, \[z0\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1q_gather_s64offset_tied, svuint64_t, uint64_t, svint64_t,
		     z0_res = svld1q_gather_s64offset_u64 (p0, x0, z0),
		     z0_res = svld1q_gather_offset (p0, x0, z0))

/*
** ld1q_gather_s64offset_untied:
**	ld1q	{z0\.q}, p0/z, \[z1\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1q_gather_s64offset_untied, svuint64_t, uint64_t, svint64_t,
		     z0_res = svld1q_gather_s64offset_u64 (p0, x0, z1),
		     z0_res = svld1q_gather_offset (p0, x0, z1))

/*
** ld1q_gather_u64offset_tied:
**	ld1q	{z0\.q}, p0/z, \[z0\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1q_gather_u64offset_tied, svuint64_t, uint64_t, svuint64_t,
		     z0_res = svld1q_gather_u64offset_u64 (p0, x0, z0),
		     z0_res = svld1q_gather_offset (p0, x0, z0))

/*
** ld1q_gather_u64offset_untied:
**	ld1q	{z0\.q}, p0/z, \[z1\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1q_gather_u64offset_untied, svuint64_t, uint64_t, svuint64_t,
		     z0_res = svld1q_gather_u64offset_u64 (p0, x0, z1),
		     z0_res = svld1q_gather_offset (p0, x0, z1))

/*
** ld1q_gather_s64index_tied: { xfail *-*-* }
**	lsl	(z[1-9][0-9]*\.d), z0\.d, #3
**	ld1q	{z0\.q}, p0/z, \[\1, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1q_gather_s64index_tied, svuint64_t, uint64_t, svint64_t,
		     z0_res = svld1q_gather_s64index_u64 (p0, x0, z0),
		     z0_res = svld1q_gather_index (p0, x0, z0))

/*
** ld1q_gather_s64index_untied:
**	lsl	(z[0-9]+\.d), z1\.d, #3
**	ld1q	{z0\.q}, p0/z, \[\1, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1q_gather_s64index_untied, svuint64_t, uint64_t, svint64_t,
		     z0_res = svld1q_gather_s64index_u64 (p0, x0, z1),
		     z0_res = svld1q_gather_index (p0, x0, z1))

/*
** ld1q_gather_u64index_tied: { xfail *-*-* }
**	lsl	(z[1-9][0-9]*\.d), z0\.d, #3
**	ld1q	{z0\.q}, p0/z, \[\1, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1q_gather_u64index_tied, svuint64_t, uint64_t, svuint64_t,
		     z0_res = svld1q_gather_u64index_u64 (p0, x0, z0),
		     z0_res = svld1q_gather_index (p0, x0, z0))

/*
** ld1q_gather_u64index_untied:
**	lsl	(z[0-9]+\.d), z1\.d, #3
**	ld1q	{z0\.q}, p0/z, \[\1, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1q_gather_u64index_untied, svuint64_t, uint64_t, svuint64_t,
		     z0_res = svld1q_gather_u64index_u64 (p0, x0, z1),
		     z0_res = svld1q_gather_index (p0, x0, z1))
