/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** st1h_scatter_u32:
**	st1h	z0\.s, p0, \[z1\.s\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_u32, svuint32_t, svuint32_t,
		       svst1h_scatter_u32base_u32 (p0, z1, z0),
		       svst1h_scatter (p0, z1, z0))

/*
** st1h_scatter_x0_u32_offset:
**	st1h	z0\.s, p0, \[x0, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_x0_u32_offset, svuint32_t, svuint32_t,
		       svst1h_scatter_u32base_offset_u32 (p0, z1, x0, z0),
		       svst1h_scatter_offset (p0, z1, x0, z0))

/*
** st1h_scatter_m2_u32_offset:
**	mov	(x[0-9]+), #?-2
**	st1h	z0\.s, p0, \[\1, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_m2_u32_offset, svuint32_t, svuint32_t,
		       svst1h_scatter_u32base_offset_u32 (p0, z1, -2, z0),
		       svst1h_scatter_offset (p0, z1, -2, z0))

/*
** st1h_scatter_0_u32_offset:
**	st1h	z0\.s, p0, \[z1\.s\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_0_u32_offset, svuint32_t, svuint32_t,
		       svst1h_scatter_u32base_offset_u32 (p0, z1, 0, z0),
		       svst1h_scatter_offset (p0, z1, 0, z0))

/*
** st1h_scatter_5_u32_offset:
**	mov	(x[0-9]+), #?5
**	st1h	z0\.s, p0, \[\1, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_5_u32_offset, svuint32_t, svuint32_t,
		       svst1h_scatter_u32base_offset_u32 (p0, z1, 5, z0),
		       svst1h_scatter_offset (p0, z1, 5, z0))

/*
** st1h_scatter_6_u32_offset:
**	st1h	z0\.s, p0, \[z1\.s, #6\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_6_u32_offset, svuint32_t, svuint32_t,
		       svst1h_scatter_u32base_offset_u32 (p0, z1, 6, z0),
		       svst1h_scatter_offset (p0, z1, 6, z0))

/*
** st1h_scatter_62_u32_offset:
**	st1h	z0\.s, p0, \[z1\.s, #62\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_62_u32_offset, svuint32_t, svuint32_t,
		       svst1h_scatter_u32base_offset_u32 (p0, z1, 62, z0),
		       svst1h_scatter_offset (p0, z1, 62, z0))

/*
** st1h_scatter_64_u32_offset:
**	mov	(x[0-9]+), #?64
**	st1h	z0\.s, p0, \[\1, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_64_u32_offset, svuint32_t, svuint32_t,
		       svst1h_scatter_u32base_offset_u32 (p0, z1, 64, z0),
		       svst1h_scatter_offset (p0, z1, 64, z0))

/*
** st1h_scatter_x0_u32_index:
**	lsl	(x[0-9]+), x0, #?1
**	st1h	z0\.s, p0, \[\1, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_x0_u32_index, svuint32_t, svuint32_t,
		       svst1h_scatter_u32base_index_u32 (p0, z1, x0, z0),
		       svst1h_scatter_index (p0, z1, x0, z0))

/*
** st1h_scatter_m1_u32_index:
**	mov	(x[0-9]+), #?-2
**	st1h	z0\.s, p0, \[\1, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_m1_u32_index, svuint32_t, svuint32_t,
		       svst1h_scatter_u32base_index_u32 (p0, z1, -1, z0),
		       svst1h_scatter_index (p0, z1, -1, z0))

/*
** st1h_scatter_0_u32_index:
**	st1h	z0\.s, p0, \[z1\.s\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_0_u32_index, svuint32_t, svuint32_t,
		       svst1h_scatter_u32base_index_u32 (p0, z1, 0, z0),
		       svst1h_scatter_index (p0, z1, 0, z0))

/*
** st1h_scatter_5_u32_index:
**	st1h	z0\.s, p0, \[z1\.s, #10\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_5_u32_index, svuint32_t, svuint32_t,
		       svst1h_scatter_u32base_index_u32 (p0, z1, 5, z0),
		       svst1h_scatter_index (p0, z1, 5, z0))

/*
** st1h_scatter_31_u32_index:
**	st1h	z0\.s, p0, \[z1\.s, #62\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_31_u32_index, svuint32_t, svuint32_t,
		       svst1h_scatter_u32base_index_u32 (p0, z1, 31, z0),
		       svst1h_scatter_index (p0, z1, 31, z0))

/*
** st1h_scatter_32_u32_index:
**	mov	(x[0-9]+), #?64
**	st1h	z0\.s, p0, \[\1, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_32_u32_index, svuint32_t, svuint32_t,
		       svst1h_scatter_u32base_index_u32 (p0, z1, 32, z0),
		       svst1h_scatter_index (p0, z1, 32, z0))

/*
** st1h_scatter_x0_u32_s32offset:
**	st1h	z0\.s, p0, \[x0, z1\.s, sxtw\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_x0_u32_s32offset, svuint32_t, uint16_t, svint32_t,
		       svst1h_scatter_s32offset_u32 (p0, x0, z1, z0),
		       svst1h_scatter_offset (p0, x0, z1, z0))

/*
** st1h_scatter_u32_s32offset:
**	st1h	z0\.s, p0, \[x0, z1\.s, sxtw\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_u32_s32offset, svuint32_t, uint16_t, svint32_t,
		       svst1h_scatter_s32offset_u32 (p0, x0, z1, z0),
		       svst1h_scatter_offset (p0, x0, z1, z0))

/*
** st1h_scatter_x0_u32_u32offset:
**	st1h	z0\.s, p0, \[x0, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_x0_u32_u32offset, svuint32_t, uint16_t, svuint32_t,
		       svst1h_scatter_u32offset_u32 (p0, x0, z1, z0),
		       svst1h_scatter_offset (p0, x0, z1, z0))

/*
** st1h_scatter_u32_u32offset:
**	st1h	z0\.s, p0, \[x0, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_u32_u32offset, svuint32_t, uint16_t, svuint32_t,
		       svst1h_scatter_u32offset_u32 (p0, x0, z1, z0),
		       svst1h_scatter_offset (p0, x0, z1, z0))

/*
** st1h_scatter_x0_u32_s32index:
**	st1h	z0\.s, p0, \[x0, z1\.s, sxtw 1\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_x0_u32_s32index, svuint32_t, uint16_t, svint32_t,
		       svst1h_scatter_s32index_u32 (p0, x0, z1, z0),
		       svst1h_scatter_index (p0, x0, z1, z0))

/*
** st1h_scatter_u32_s32index:
**	st1h	z0\.s, p0, \[x0, z1\.s, sxtw 1\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_u32_s32index, svuint32_t, uint16_t, svint32_t,
		       svst1h_scatter_s32index_u32 (p0, x0, z1, z0),
		       svst1h_scatter_index (p0, x0, z1, z0))

/*
** st1h_scatter_x0_u32_u32index:
**	st1h	z0\.s, p0, \[x0, z1\.s, uxtw 1\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_x0_u32_u32index, svuint32_t, uint16_t, svuint32_t,
		       svst1h_scatter_u32index_u32 (p0, x0, z1, z0),
		       svst1h_scatter_index (p0, x0, z1, z0))

/*
** st1h_scatter_u32_u32index:
**	st1h	z0\.s, p0, \[x0, z1\.s, uxtw 1\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_u32_u32index, svuint32_t, uint16_t, svuint32_t,
		       svst1h_scatter_u32index_u32 (p0, x0, z1, z0),
		       svst1h_scatter_index (p0, x0, z1, z0))
