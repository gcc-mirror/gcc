/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** st1_scatter_u32:
**	st1w	z0\.s, p0, \[z1\.s\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_u32, svuint32_t, svuint32_t,
		       svst1_scatter_u32base_u32 (p0, z1, z0),
		       svst1_scatter (p0, z1, z0))

/*
** st1_scatter_x0_u32_offset:
**	st1w	z0\.s, p0, \[x0, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_x0_u32_offset, svuint32_t, svuint32_t,
		       svst1_scatter_u32base_offset_u32 (p0, z1, x0, z0),
		       svst1_scatter_offset (p0, z1, x0, z0))

/*
** st1_scatter_m4_u32_offset:
**	mov	(x[0-9]+), #?-4
**	st1w	z0\.s, p0, \[\1, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_m4_u32_offset, svuint32_t, svuint32_t,
		       svst1_scatter_u32base_offset_u32 (p0, z1, -4, z0),
		       svst1_scatter_offset (p0, z1, -4, z0))

/*
** st1_scatter_0_u32_offset:
**	st1w	z0\.s, p0, \[z1\.s\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_0_u32_offset, svuint32_t, svuint32_t,
		       svst1_scatter_u32base_offset_u32 (p0, z1, 0, z0),
		       svst1_scatter_offset (p0, z1, 0, z0))

/*
** st1_scatter_5_u32_offset:
**	mov	(x[0-9]+), #?5
**	st1w	z0\.s, p0, \[\1, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_5_u32_offset, svuint32_t, svuint32_t,
		       svst1_scatter_u32base_offset_u32 (p0, z1, 5, z0),
		       svst1_scatter_offset (p0, z1, 5, z0))

/*
** st1_scatter_6_u32_offset:
**	mov	(x[0-9]+), #?6
**	st1w	z0\.s, p0, \[\1, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_6_u32_offset, svuint32_t, svuint32_t,
		       svst1_scatter_u32base_offset_u32 (p0, z1, 6, z0),
		       svst1_scatter_offset (p0, z1, 6, z0))

/*
** st1_scatter_7_u32_offset:
**	mov	(x[0-9]+), #?7
**	st1w	z0\.s, p0, \[\1, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_7_u32_offset, svuint32_t, svuint32_t,
		       svst1_scatter_u32base_offset_u32 (p0, z1, 7, z0),
		       svst1_scatter_offset (p0, z1, 7, z0))

/*
** st1_scatter_8_u32_offset:
**	st1w	z0\.s, p0, \[z1\.s, #8\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_8_u32_offset, svuint32_t, svuint32_t,
		       svst1_scatter_u32base_offset_u32 (p0, z1, 8, z0),
		       svst1_scatter_offset (p0, z1, 8, z0))

/*
** st1_scatter_124_u32_offset:
**	st1w	z0\.s, p0, \[z1\.s, #124\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_124_u32_offset, svuint32_t, svuint32_t,
		       svst1_scatter_u32base_offset_u32 (p0, z1, 124, z0),
		       svst1_scatter_offset (p0, z1, 124, z0))

/*
** st1_scatter_128_u32_offset:
**	mov	(x[0-9]+), #?128
**	st1w	z0\.s, p0, \[\1, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_128_u32_offset, svuint32_t, svuint32_t,
		       svst1_scatter_u32base_offset_u32 (p0, z1, 128, z0),
		       svst1_scatter_offset (p0, z1, 128, z0))

/*
** st1_scatter_x0_u32_index:
**	lsl	(x[0-9]+), x0, #?2
**	st1w	z0\.s, p0, \[\1, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_x0_u32_index, svuint32_t, svuint32_t,
		       svst1_scatter_u32base_index_u32 (p0, z1, x0, z0),
		       svst1_scatter_index (p0, z1, x0, z0))

/*
** st1_scatter_m1_u32_index:
**	mov	(x[0-9]+), #?-4
**	st1w	z0\.s, p0, \[\1, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_m1_u32_index, svuint32_t, svuint32_t,
		       svst1_scatter_u32base_index_u32 (p0, z1, -1, z0),
		       svst1_scatter_index (p0, z1, -1, z0))

/*
** st1_scatter_0_u32_index:
**	st1w	z0\.s, p0, \[z1\.s\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_0_u32_index, svuint32_t, svuint32_t,
		       svst1_scatter_u32base_index_u32 (p0, z1, 0, z0),
		       svst1_scatter_index (p0, z1, 0, z0))

/*
** st1_scatter_5_u32_index:
**	st1w	z0\.s, p0, \[z1\.s, #20\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_5_u32_index, svuint32_t, svuint32_t,
		       svst1_scatter_u32base_index_u32 (p0, z1, 5, z0),
		       svst1_scatter_index (p0, z1, 5, z0))

/*
** st1_scatter_31_u32_index:
**	st1w	z0\.s, p0, \[z1\.s, #124\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_31_u32_index, svuint32_t, svuint32_t,
		       svst1_scatter_u32base_index_u32 (p0, z1, 31, z0),
		       svst1_scatter_index (p0, z1, 31, z0))

/*
** st1_scatter_32_u32_index:
**	mov	(x[0-9]+), #?128
**	st1w	z0\.s, p0, \[\1, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_32_u32_index, svuint32_t, svuint32_t,
		       svst1_scatter_u32base_index_u32 (p0, z1, 32, z0),
		       svst1_scatter_index (p0, z1, 32, z0))

/*
** st1_scatter_x0_u32_s32offset:
**	st1w	z0\.s, p0, \[x0, z1\.s, sxtw\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_x0_u32_s32offset, svuint32_t, uint32_t, svint32_t,
		       svst1_scatter_s32offset_u32 (p0, x0, z1, z0),
		       svst1_scatter_offset (p0, x0, z1, z0))

/*
** st1_scatter_u32_s32offset:
**	st1w	z0\.s, p0, \[x0, z1\.s, sxtw\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_u32_s32offset, svuint32_t, uint32_t, svint32_t,
		       svst1_scatter_s32offset_u32 (p0, x0, z1, z0),
		       svst1_scatter_offset (p0, x0, z1, z0))

/*
** st1_scatter_x0_u32_u32offset:
**	st1w	z0\.s, p0, \[x0, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_x0_u32_u32offset, svuint32_t, uint32_t, svuint32_t,
		       svst1_scatter_u32offset_u32 (p0, x0, z1, z0),
		       svst1_scatter_offset (p0, x0, z1, z0))

/*
** st1_scatter_u32_u32offset:
**	st1w	z0\.s, p0, \[x0, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_u32_u32offset, svuint32_t, uint32_t, svuint32_t,
		       svst1_scatter_u32offset_u32 (p0, x0, z1, z0),
		       svst1_scatter_offset (p0, x0, z1, z0))

/*
** st1_scatter_x0_u32_s32index:
**	st1w	z0\.s, p0, \[x0, z1\.s, sxtw 2\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_x0_u32_s32index, svuint32_t, uint32_t, svint32_t,
		       svst1_scatter_s32index_u32 (p0, x0, z1, z0),
		       svst1_scatter_index (p0, x0, z1, z0))

/*
** st1_scatter_u32_s32index:
**	st1w	z0\.s, p0, \[x0, z1\.s, sxtw 2\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_u32_s32index, svuint32_t, uint32_t, svint32_t,
		       svst1_scatter_s32index_u32 (p0, x0, z1, z0),
		       svst1_scatter_index (p0, x0, z1, z0))

/*
** st1_scatter_x0_u32_u32index:
**	st1w	z0\.s, p0, \[x0, z1\.s, uxtw 2\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_x0_u32_u32index, svuint32_t, uint32_t, svuint32_t,
		       svst1_scatter_u32index_u32 (p0, x0, z1, z0),
		       svst1_scatter_index (p0, x0, z1, z0))

/*
** st1_scatter_u32_u32index:
**	st1w	z0\.s, p0, \[x0, z1\.s, uxtw 2\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_u32_u32index, svuint32_t, uint32_t, svuint32_t,
		       svst1_scatter_u32index_u32 (p0, x0, z1, z0),
		       svst1_scatter_index (p0, x0, z1, z0))
