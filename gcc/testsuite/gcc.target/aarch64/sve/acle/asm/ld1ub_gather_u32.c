/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ld1ub_gather_u32_tied1:
**	ld1b	z0\.s, p0/z, \[z0\.s\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1ub_gather_u32_tied1, svuint32_t, svuint32_t,
		     z0_res = svld1ub_gather_u32base_u32 (p0, z0),
		     z0_res = svld1ub_gather_u32 (p0, z0))

/*
** ld1ub_gather_u32_untied:
**	ld1b	z0\.s, p0/z, \[z1\.s\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1ub_gather_u32_untied, svuint32_t, svuint32_t,
		     z0_res = svld1ub_gather_u32base_u32 (p0, z1),
		     z0_res = svld1ub_gather_u32 (p0, z1))

/*
** ld1ub_gather_x0_u32_offset:
**	ld1b	z0\.s, p0/z, \[x0, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1ub_gather_x0_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svld1ub_gather_u32base_offset_u32 (p0, z0, x0),
		     z0_res = svld1ub_gather_offset_u32 (p0, z0, x0))

/*
** ld1ub_gather_m1_u32_offset:
**	mov	(x[0-9]+), #?-1
**	ld1b	z0\.s, p0/z, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1ub_gather_m1_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svld1ub_gather_u32base_offset_u32 (p0, z0, -1),
		     z0_res = svld1ub_gather_offset_u32 (p0, z0, -1))

/*
** ld1ub_gather_0_u32_offset:
**	ld1b	z0\.s, p0/z, \[z0\.s\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1ub_gather_0_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svld1ub_gather_u32base_offset_u32 (p0, z0, 0),
		     z0_res = svld1ub_gather_offset_u32 (p0, z0, 0))

/*
** ld1ub_gather_5_u32_offset:
**	ld1b	z0\.s, p0/z, \[z0\.s, #5\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1ub_gather_5_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svld1ub_gather_u32base_offset_u32 (p0, z0, 5),
		     z0_res = svld1ub_gather_offset_u32 (p0, z0, 5))

/*
** ld1ub_gather_31_u32_offset:
**	ld1b	z0\.s, p0/z, \[z0\.s, #31\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1ub_gather_31_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svld1ub_gather_u32base_offset_u32 (p0, z0, 31),
		     z0_res = svld1ub_gather_offset_u32 (p0, z0, 31))

/*
** ld1ub_gather_32_u32_offset:
**	mov	(x[0-9]+), #?32
**	ld1b	z0\.s, p0/z, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1ub_gather_32_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svld1ub_gather_u32base_offset_u32 (p0, z0, 32),
		     z0_res = svld1ub_gather_offset_u32 (p0, z0, 32))

/*
** ld1ub_gather_x0_u32_s32offset:
**	ld1b	z0\.s, p0/z, \[x0, z0\.s, sxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1ub_gather_x0_u32_s32offset, svuint32_t, uint8_t, svint32_t,
		     z0_res = svld1ub_gather_s32offset_u32 (p0, x0, z0),
		     z0_res = svld1ub_gather_offset_u32 (p0, x0, z0))

/*
** ld1ub_gather_tied1_u32_s32offset:
**	ld1b	z0\.s, p0/z, \[x0, z0\.s, sxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1ub_gather_tied1_u32_s32offset, svuint32_t, uint8_t, svint32_t,
		     z0_res = svld1ub_gather_s32offset_u32 (p0, x0, z0),
		     z0_res = svld1ub_gather_offset_u32 (p0, x0, z0))

/*
** ld1ub_gather_untied_u32_s32offset:
**	ld1b	z0\.s, p0/z, \[x0, z1\.s, sxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1ub_gather_untied_u32_s32offset, svuint32_t, uint8_t, svint32_t,
		     z0_res = svld1ub_gather_s32offset_u32 (p0, x0, z1),
		     z0_res = svld1ub_gather_offset_u32 (p0, x0, z1))

/*
** ld1ub_gather_x0_u32_u32offset:
**	ld1b	z0\.s, p0/z, \[x0, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1ub_gather_x0_u32_u32offset, svuint32_t, uint8_t, svuint32_t,
		     z0_res = svld1ub_gather_u32offset_u32 (p0, x0, z0),
		     z0_res = svld1ub_gather_offset_u32 (p0, x0, z0))

/*
** ld1ub_gather_tied1_u32_u32offset:
**	ld1b	z0\.s, p0/z, \[x0, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1ub_gather_tied1_u32_u32offset, svuint32_t, uint8_t, svuint32_t,
		     z0_res = svld1ub_gather_u32offset_u32 (p0, x0, z0),
		     z0_res = svld1ub_gather_offset_u32 (p0, x0, z0))

/*
** ld1ub_gather_untied_u32_u32offset:
**	ld1b	z0\.s, p0/z, \[x0, z1\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1ub_gather_untied_u32_u32offset, svuint32_t, uint8_t, svuint32_t,
		     z0_res = svld1ub_gather_u32offset_u32 (p0, x0, z1),
		     z0_res = svld1ub_gather_offset_u32 (p0, x0, z1))
