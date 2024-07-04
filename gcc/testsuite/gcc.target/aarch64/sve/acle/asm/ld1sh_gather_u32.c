/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ld1sh_gather_u32_tied1:
**	ld1sh	z0\.s, p0/z, \[z0\.s\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1sh_gather_u32_tied1, svuint32_t, svuint32_t,
		     z0_res = svld1sh_gather_u32base_u32 (p0, z0),
		     z0_res = svld1sh_gather_u32 (p0, z0))

/*
** ld1sh_gather_u32_untied:
**	ld1sh	z0\.s, p0/z, \[z1\.s\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1sh_gather_u32_untied, svuint32_t, svuint32_t,
		     z0_res = svld1sh_gather_u32base_u32 (p0, z1),
		     z0_res = svld1sh_gather_u32 (p0, z1))

/*
** ld1sh_gather_x0_u32_offset:
**	ld1sh	z0\.s, p0/z, \[x0, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1sh_gather_x0_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svld1sh_gather_u32base_offset_u32 (p0, z0, x0),
		     z0_res = svld1sh_gather_offset_u32 (p0, z0, x0))

/*
** ld1sh_gather_m2_u32_offset:
**	mov	(x[0-9]+), #?-2
**	ld1sh	z0\.s, p0/z, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1sh_gather_m2_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svld1sh_gather_u32base_offset_u32 (p0, z0, -2),
		     z0_res = svld1sh_gather_offset_u32 (p0, z0, -2))

/*
** ld1sh_gather_0_u32_offset:
**	ld1sh	z0\.s, p0/z, \[z0\.s\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1sh_gather_0_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svld1sh_gather_u32base_offset_u32 (p0, z0, 0),
		     z0_res = svld1sh_gather_offset_u32 (p0, z0, 0))

/*
** ld1sh_gather_5_u32_offset:
**	mov	(x[0-9]+), #?5
**	ld1sh	z0\.s, p0/z, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1sh_gather_5_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svld1sh_gather_u32base_offset_u32 (p0, z0, 5),
		     z0_res = svld1sh_gather_offset_u32 (p0, z0, 5))

/*
** ld1sh_gather_6_u32_offset:
**	ld1sh	z0\.s, p0/z, \[z0\.s, #6\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1sh_gather_6_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svld1sh_gather_u32base_offset_u32 (p0, z0, 6),
		     z0_res = svld1sh_gather_offset_u32 (p0, z0, 6))

/*
** ld1sh_gather_62_u32_offset:
**	ld1sh	z0\.s, p0/z, \[z0\.s, #62\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1sh_gather_62_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svld1sh_gather_u32base_offset_u32 (p0, z0, 62),
		     z0_res = svld1sh_gather_offset_u32 (p0, z0, 62))

/*
** ld1sh_gather_64_u32_offset:
**	mov	(x[0-9]+), #?64
**	ld1sh	z0\.s, p0/z, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1sh_gather_64_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svld1sh_gather_u32base_offset_u32 (p0, z0, 64),
		     z0_res = svld1sh_gather_offset_u32 (p0, z0, 64))

/*
** ld1sh_gather_x0_u32_index:
**	lsl	(x[0-9]+), x0, #?1
**	ld1sh	z0\.s, p0/z, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1sh_gather_x0_u32_index, svuint32_t, svuint32_t,
		     z0_res = svld1sh_gather_u32base_index_u32 (p0, z0, x0),
		     z0_res = svld1sh_gather_index_u32 (p0, z0, x0))

/*
** ld1sh_gather_m1_u32_index:
**	mov	(x[0-9]+), #?-2
**	ld1sh	z0\.s, p0/z, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1sh_gather_m1_u32_index, svuint32_t, svuint32_t,
		     z0_res = svld1sh_gather_u32base_index_u32 (p0, z0, -1),
		     z0_res = svld1sh_gather_index_u32 (p0, z0, -1))

/*
** ld1sh_gather_0_u32_index:
**	ld1sh	z0\.s, p0/z, \[z0\.s\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1sh_gather_0_u32_index, svuint32_t, svuint32_t,
		     z0_res = svld1sh_gather_u32base_index_u32 (p0, z0, 0),
		     z0_res = svld1sh_gather_index_u32 (p0, z0, 0))

/*
** ld1sh_gather_5_u32_index:
**	ld1sh	z0\.s, p0/z, \[z0\.s, #10\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1sh_gather_5_u32_index, svuint32_t, svuint32_t,
		     z0_res = svld1sh_gather_u32base_index_u32 (p0, z0, 5),
		     z0_res = svld1sh_gather_index_u32 (p0, z0, 5))

/*
** ld1sh_gather_31_u32_index:
**	ld1sh	z0\.s, p0/z, \[z0\.s, #62\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1sh_gather_31_u32_index, svuint32_t, svuint32_t,
		     z0_res = svld1sh_gather_u32base_index_u32 (p0, z0, 31),
		     z0_res = svld1sh_gather_index_u32 (p0, z0, 31))

/*
** ld1sh_gather_32_u32_index:
**	mov	(x[0-9]+), #?64
**	ld1sh	z0\.s, p0/z, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1sh_gather_32_u32_index, svuint32_t, svuint32_t,
		     z0_res = svld1sh_gather_u32base_index_u32 (p0, z0, 32),
		     z0_res = svld1sh_gather_index_u32 (p0, z0, 32))

/*
** ld1sh_gather_x0_u32_s32offset:
**	ld1sh	z0\.s, p0/z, \[x0, z0\.s, sxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1sh_gather_x0_u32_s32offset, svuint32_t, int16_t, svint32_t,
		     z0_res = svld1sh_gather_s32offset_u32 (p0, x0, z0),
		     z0_res = svld1sh_gather_offset_u32 (p0, x0, z0))

/*
** ld1sh_gather_tied1_u32_s32offset:
**	ld1sh	z0\.s, p0/z, \[x0, z0\.s, sxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1sh_gather_tied1_u32_s32offset, svuint32_t, int16_t, svint32_t,
		     z0_res = svld1sh_gather_s32offset_u32 (p0, x0, z0),
		     z0_res = svld1sh_gather_offset_u32 (p0, x0, z0))

/*
** ld1sh_gather_untied_u32_s32offset:
**	ld1sh	z0\.s, p0/z, \[x0, z1\.s, sxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1sh_gather_untied_u32_s32offset, svuint32_t, int16_t, svint32_t,
		     z0_res = svld1sh_gather_s32offset_u32 (p0, x0, z1),
		     z0_res = svld1sh_gather_offset_u32 (p0, x0, z1))

/*
** ld1sh_gather_x0_u32_u32offset:
**	ld1sh	z0\.s, p0/z, \[x0, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1sh_gather_x0_u32_u32offset, svuint32_t, int16_t, svuint32_t,
		     z0_res = svld1sh_gather_u32offset_u32 (p0, x0, z0),
		     z0_res = svld1sh_gather_offset_u32 (p0, x0, z0))

/*
** ld1sh_gather_tied1_u32_u32offset:
**	ld1sh	z0\.s, p0/z, \[x0, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1sh_gather_tied1_u32_u32offset, svuint32_t, int16_t, svuint32_t,
		     z0_res = svld1sh_gather_u32offset_u32 (p0, x0, z0),
		     z0_res = svld1sh_gather_offset_u32 (p0, x0, z0))

/*
** ld1sh_gather_untied_u32_u32offset:
**	ld1sh	z0\.s, p0/z, \[x0, z1\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1sh_gather_untied_u32_u32offset, svuint32_t, int16_t, svuint32_t,
		     z0_res = svld1sh_gather_u32offset_u32 (p0, x0, z1),
		     z0_res = svld1sh_gather_offset_u32 (p0, x0, z1))

/*
** ld1sh_gather_x0_u32_s32index:
**	ld1sh	z0\.s, p0/z, \[x0, z0\.s, sxtw 1\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1sh_gather_x0_u32_s32index, svuint32_t, int16_t, svint32_t,
		     z0_res = svld1sh_gather_s32index_u32 (p0, x0, z0),
		     z0_res = svld1sh_gather_index_u32 (p0, x0, z0))

/*
** ld1sh_gather_tied1_u32_s32index:
**	ld1sh	z0\.s, p0/z, \[x0, z0\.s, sxtw 1\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1sh_gather_tied1_u32_s32index, svuint32_t, int16_t, svint32_t,
		     z0_res = svld1sh_gather_s32index_u32 (p0, x0, z0),
		     z0_res = svld1sh_gather_index_u32 (p0, x0, z0))

/*
** ld1sh_gather_untied_u32_s32index:
**	ld1sh	z0\.s, p0/z, \[x0, z1\.s, sxtw 1\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1sh_gather_untied_u32_s32index, svuint32_t, int16_t, svint32_t,
		     z0_res = svld1sh_gather_s32index_u32 (p0, x0, z1),
		     z0_res = svld1sh_gather_index_u32 (p0, x0, z1))

/*
** ld1sh_gather_x0_u32_u32index:
**	ld1sh	z0\.s, p0/z, \[x0, z0\.s, uxtw 1\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1sh_gather_x0_u32_u32index, svuint32_t, int16_t, svuint32_t,
		     z0_res = svld1sh_gather_u32index_u32 (p0, x0, z0),
		     z0_res = svld1sh_gather_index_u32 (p0, x0, z0))

/*
** ld1sh_gather_tied1_u32_u32index:
**	ld1sh	z0\.s, p0/z, \[x0, z0\.s, uxtw 1\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1sh_gather_tied1_u32_u32index, svuint32_t, int16_t, svuint32_t,
		     z0_res = svld1sh_gather_u32index_u32 (p0, x0, z0),
		     z0_res = svld1sh_gather_index_u32 (p0, x0, z0))

/*
** ld1sh_gather_untied_u32_u32index:
**	ld1sh	z0\.s, p0/z, \[x0, z1\.s, uxtw 1\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1sh_gather_untied_u32_u32index, svuint32_t, int16_t, svuint32_t,
		     z0_res = svld1sh_gather_u32index_u32 (p0, x0, z1),
		     z0_res = svld1sh_gather_index_u32 (p0, x0, z1))
