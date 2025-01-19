/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** ld1q_gather_tied1:
**	ld1q	{z0\.q}, p0/z, \[z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1q_gather_tied1, svuint8_t, svuint64_t,
		     z0_res = svld1q_gather_u64base_u8 (p0, z0),
		     z0_res = svld1q_gather_u8 (p0, z0))

/*
** ld1q_gather_untied:
**	ld1q	{z0\.q}, p0/z, \[z1\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1q_gather_untied, svuint8_t, svuint64_t,
		     z0_res = svld1q_gather_u64base_u8 (p0, z1),
		     z0_res = svld1q_gather_u8 (p0, z1))

/*
** ld1q_gather_x0_offset_tied:
**	ld1q	{z0\.q}, p0/z, \[z0\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1q_gather_x0_offset_tied, svuint8_t, svuint64_t,
		     z0_res = svld1q_gather_u64base_offset_u8 (p0, z0, x0),
		     z0_res = svld1q_gather_offset_u8 (p0, z0, x0))

/*
** ld1q_gather_x0_offset_untied:
**	ld1q	{z0\.q}, p0/z, \[z1\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1q_gather_x0_offset_untied, svuint8_t, svuint64_t,
		     z0_res = svld1q_gather_u64base_offset_u8 (p0, z1, x0),
		     z0_res = svld1q_gather_offset_u8 (p0, z1, x0))

/*
** ld1q_gather_m16_offset:
**	mov	(x[0-9]+), #?-16
**	ld1q	{z0\.q}, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1q_gather_m16_offset, svuint8_t, svuint64_t,
		     z0_res = svld1q_gather_u64base_offset_u8 (p0, z0, -16),
		     z0_res = svld1q_gather_offset_u8 (p0, z0, -16))

/*
** ld1q_gather_0_offset:
**	ld1q	{z0\.q}, p0/z, \[z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1q_gather_0_offset, svuint8_t, svuint64_t,
		     z0_res = svld1q_gather_u64base_offset_u8 (p0, z0, 0),
		     z0_res = svld1q_gather_offset_u8 (p0, z0, 0))

/*
** ld1q_gather_16_offset:
**	mov	(x[0-9]+), #?16
**	ld1q	{z0\.q}, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1q_gather_16_offset, svuint8_t, svuint64_t,
		     z0_res = svld1q_gather_u64base_offset_u8 (p0, z0, 16),
		     z0_res = svld1q_gather_offset_u8 (p0, z0, 16))

/*
** ld1q_gather_s64offset_tied:
**	ld1q	{z0\.q}, p0/z, \[z0\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1q_gather_s64offset_tied, svuint8_t, uint8_t, svint64_t,
		     z0_res = svld1q_gather_s64offset_u8 (p0, x0, z0),
		     z0_res = svld1q_gather_offset (p0, x0, z0))

/*
** ld1q_gather_s64offset_untied:
**	ld1q	{z0\.q}, p0/z, \[z1\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1q_gather_s64offset_untied, svuint8_t, uint8_t, svint64_t,
		     z0_res = svld1q_gather_s64offset_u8 (p0, x0, z1),
		     z0_res = svld1q_gather_offset (p0, x0, z1))

/*
** ld1q_gather_u64offset_tied:
**	ld1q	{z0\.q}, p0/z, \[z0\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1q_gather_u64offset_tied, svuint8_t, uint8_t, svuint64_t,
		     z0_res = svld1q_gather_u64offset_u8 (p0, x0, z0),
		     z0_res = svld1q_gather_offset (p0, x0, z0))

/*
** ld1q_gather_u64offset_untied:
**	ld1q	{z0\.q}, p0/z, \[z1\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1q_gather_u64offset_untied, svuint8_t, uint8_t, svuint64_t,
		     z0_res = svld1q_gather_u64offset_u8 (p0, x0, z1),
		     z0_res = svld1q_gather_offset (p0, x0, z1))
