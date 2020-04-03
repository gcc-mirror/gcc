/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ld1_gather_f32_tied1:
**	ld1w	z0\.s, p0/z, \[z0\.s\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_f32_tied1, svfloat32_t, svuint32_t,
		     z0_res = svld1_gather_u32base_f32 (p0, z0),
		     z0_res = svld1_gather_f32 (p0, z0))

/*
** ld1_gather_f32_untied:
**	ld1w	z0\.s, p0/z, \[z1\.s\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_f32_untied, svfloat32_t, svuint32_t,
		     z0_res = svld1_gather_u32base_f32 (p0, z1),
		     z0_res = svld1_gather_f32 (p0, z1))

/*
** ld1_gather_x0_f32_offset:
**	ld1w	z0\.s, p0/z, \[x0, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_x0_f32_offset, svfloat32_t, svuint32_t,
		     z0_res = svld1_gather_u32base_offset_f32 (p0, z0, x0),
		     z0_res = svld1_gather_offset_f32 (p0, z0, x0))

/*
** ld1_gather_m4_f32_offset:
**	mov	(x[0-9]+), #?-4
**	ld1w	z0\.s, p0/z, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_m4_f32_offset, svfloat32_t, svuint32_t,
		     z0_res = svld1_gather_u32base_offset_f32 (p0, z0, -4),
		     z0_res = svld1_gather_offset_f32 (p0, z0, -4))

/*
** ld1_gather_0_f32_offset:
**	ld1w	z0\.s, p0/z, \[z0\.s\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_0_f32_offset, svfloat32_t, svuint32_t,
		     z0_res = svld1_gather_u32base_offset_f32 (p0, z0, 0),
		     z0_res = svld1_gather_offset_f32 (p0, z0, 0))

/*
** ld1_gather_5_f32_offset:
**	mov	(x[0-9]+), #?5
**	ld1w	z0\.s, p0/z, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_5_f32_offset, svfloat32_t, svuint32_t,
		     z0_res = svld1_gather_u32base_offset_f32 (p0, z0, 5),
		     z0_res = svld1_gather_offset_f32 (p0, z0, 5))

/*
** ld1_gather_6_f32_offset:
**	mov	(x[0-9]+), #?6
**	ld1w	z0\.s, p0/z, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_6_f32_offset, svfloat32_t, svuint32_t,
		     z0_res = svld1_gather_u32base_offset_f32 (p0, z0, 6),
		     z0_res = svld1_gather_offset_f32 (p0, z0, 6))

/*
** ld1_gather_7_f32_offset:
**	mov	(x[0-9]+), #?7
**	ld1w	z0\.s, p0/z, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_7_f32_offset, svfloat32_t, svuint32_t,
		     z0_res = svld1_gather_u32base_offset_f32 (p0, z0, 7),
		     z0_res = svld1_gather_offset_f32 (p0, z0, 7))

/*
** ld1_gather_8_f32_offset:
**	ld1w	z0\.s, p0/z, \[z0\.s, #8\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_8_f32_offset, svfloat32_t, svuint32_t,
		     z0_res = svld1_gather_u32base_offset_f32 (p0, z0, 8),
		     z0_res = svld1_gather_offset_f32 (p0, z0, 8))

/*
** ld1_gather_124_f32_offset:
**	ld1w	z0\.s, p0/z, \[z0\.s, #124\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_124_f32_offset, svfloat32_t, svuint32_t,
		     z0_res = svld1_gather_u32base_offset_f32 (p0, z0, 124),
		     z0_res = svld1_gather_offset_f32 (p0, z0, 124))

/*
** ld1_gather_128_f32_offset:
**	mov	(x[0-9]+), #?128
**	ld1w	z0\.s, p0/z, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_128_f32_offset, svfloat32_t, svuint32_t,
		     z0_res = svld1_gather_u32base_offset_f32 (p0, z0, 128),
		     z0_res = svld1_gather_offset_f32 (p0, z0, 128))

/*
** ld1_gather_x0_f32_index:
**	lsl	(x[0-9]+), x0, #?2
**	ld1w	z0\.s, p0/z, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_x0_f32_index, svfloat32_t, svuint32_t,
		     z0_res = svld1_gather_u32base_index_f32 (p0, z0, x0),
		     z0_res = svld1_gather_index_f32 (p0, z0, x0))

/*
** ld1_gather_m1_f32_index:
**	mov	(x[0-9]+), #?-4
**	ld1w	z0\.s, p0/z, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_m1_f32_index, svfloat32_t, svuint32_t,
		     z0_res = svld1_gather_u32base_index_f32 (p0, z0, -1),
		     z0_res = svld1_gather_index_f32 (p0, z0, -1))

/*
** ld1_gather_0_f32_index:
**	ld1w	z0\.s, p0/z, \[z0\.s\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_0_f32_index, svfloat32_t, svuint32_t,
		     z0_res = svld1_gather_u32base_index_f32 (p0, z0, 0),
		     z0_res = svld1_gather_index_f32 (p0, z0, 0))

/*
** ld1_gather_5_f32_index:
**	ld1w	z0\.s, p0/z, \[z0\.s, #20\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_5_f32_index, svfloat32_t, svuint32_t,
		     z0_res = svld1_gather_u32base_index_f32 (p0, z0, 5),
		     z0_res = svld1_gather_index_f32 (p0, z0, 5))

/*
** ld1_gather_31_f32_index:
**	ld1w	z0\.s, p0/z, \[z0\.s, #124\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_31_f32_index, svfloat32_t, svuint32_t,
		     z0_res = svld1_gather_u32base_index_f32 (p0, z0, 31),
		     z0_res = svld1_gather_index_f32 (p0, z0, 31))

/*
** ld1_gather_32_f32_index:
**	mov	(x[0-9]+), #?128
**	ld1w	z0\.s, p0/z, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_32_f32_index, svfloat32_t, svuint32_t,
		     z0_res = svld1_gather_u32base_index_f32 (p0, z0, 32),
		     z0_res = svld1_gather_index_f32 (p0, z0, 32))

/*
** ld1_gather_x0_f32_s32offset:
**	ld1w	z0\.s, p0/z, \[x0, z0\.s, sxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_x0_f32_s32offset, svfloat32_t, float32_t, svint32_t,
		     z0_res = svld1_gather_s32offset_f32 (p0, x0, z0),
		     z0_res = svld1_gather_offset (p0, x0, z0))

/*
** ld1_gather_tied1_f32_s32offset:
**	ld1w	z0\.s, p0/z, \[x0, z0\.s, sxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_tied1_f32_s32offset, svfloat32_t, float32_t, svint32_t,
		     z0_res = svld1_gather_s32offset_f32 (p0, x0, z0),
		     z0_res = svld1_gather_offset (p0, x0, z0))

/*
** ld1_gather_untied_f32_s32offset:
**	ld1w	z0\.s, p0/z, \[x0, z1\.s, sxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_untied_f32_s32offset, svfloat32_t, float32_t, svint32_t,
		     z0_res = svld1_gather_s32offset_f32 (p0, x0, z1),
		     z0_res = svld1_gather_offset (p0, x0, z1))

/*
** ld1_gather_x0_f32_u32offset:
**	ld1w	z0\.s, p0/z, \[x0, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_x0_f32_u32offset, svfloat32_t, float32_t, svuint32_t,
		     z0_res = svld1_gather_u32offset_f32 (p0, x0, z0),
		     z0_res = svld1_gather_offset (p0, x0, z0))

/*
** ld1_gather_tied1_f32_u32offset:
**	ld1w	z0\.s, p0/z, \[x0, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_tied1_f32_u32offset, svfloat32_t, float32_t, svuint32_t,
		     z0_res = svld1_gather_u32offset_f32 (p0, x0, z0),
		     z0_res = svld1_gather_offset (p0, x0, z0))

/*
** ld1_gather_untied_f32_u32offset:
**	ld1w	z0\.s, p0/z, \[x0, z1\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_untied_f32_u32offset, svfloat32_t, float32_t, svuint32_t,
		     z0_res = svld1_gather_u32offset_f32 (p0, x0, z1),
		     z0_res = svld1_gather_offset (p0, x0, z1))

/*
** ld1_gather_x0_f32_s32index:
**	ld1w	z0\.s, p0/z, \[x0, z0\.s, sxtw 2\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_x0_f32_s32index, svfloat32_t, float32_t, svint32_t,
		     z0_res = svld1_gather_s32index_f32 (p0, x0, z0),
		     z0_res = svld1_gather_index (p0, x0, z0))

/*
** ld1_gather_tied1_f32_s32index:
**	ld1w	z0\.s, p0/z, \[x0, z0\.s, sxtw 2\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_tied1_f32_s32index, svfloat32_t, float32_t, svint32_t,
		     z0_res = svld1_gather_s32index_f32 (p0, x0, z0),
		     z0_res = svld1_gather_index (p0, x0, z0))

/*
** ld1_gather_untied_f32_s32index:
**	ld1w	z0\.s, p0/z, \[x0, z1\.s, sxtw 2\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_untied_f32_s32index, svfloat32_t, float32_t, svint32_t,
		     z0_res = svld1_gather_s32index_f32 (p0, x0, z1),
		     z0_res = svld1_gather_index (p0, x0, z1))

/*
** ld1_gather_x0_f32_u32index:
**	ld1w	z0\.s, p0/z, \[x0, z0\.s, uxtw 2\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_x0_f32_u32index, svfloat32_t, float32_t, svuint32_t,
		     z0_res = svld1_gather_u32index_f32 (p0, x0, z0),
		     z0_res = svld1_gather_index (p0, x0, z0))

/*
** ld1_gather_tied1_f32_u32index:
**	ld1w	z0\.s, p0/z, \[x0, z0\.s, uxtw 2\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_tied1_f32_u32index, svfloat32_t, float32_t, svuint32_t,
		     z0_res = svld1_gather_u32index_f32 (p0, x0, z0),
		     z0_res = svld1_gather_index (p0, x0, z0))

/*
** ld1_gather_untied_f32_u32index:
**	ld1w	z0\.s, p0/z, \[x0, z1\.s, uxtw 2\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_untied_f32_u32index, svfloat32_t, float32_t, svuint32_t,
		     z0_res = svld1_gather_u32index_f32 (p0, x0, z1),
		     z0_res = svld1_gather_index (p0, x0, z1))
