/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ldff1sb_gather_u32_tied1:
**	ldff1sb	z0\.s, p0/z, \[z0\.s\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1sb_gather_u32_tied1, svuint32_t, svuint32_t,
		     z0_res = svldff1sb_gather_u32base_u32 (p0, z0),
		     z0_res = svldff1sb_gather_u32 (p0, z0))

/*
** ldff1sb_gather_u32_untied:
**	ldff1sb	z0\.s, p0/z, \[z1\.s\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1sb_gather_u32_untied, svuint32_t, svuint32_t,
		     z0_res = svldff1sb_gather_u32base_u32 (p0, z1),
		     z0_res = svldff1sb_gather_u32 (p0, z1))

/*
** ldff1sb_gather_x0_u32_offset:
**	ldff1sb	z0\.s, p0/z, \[x0, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1sb_gather_x0_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svldff1sb_gather_u32base_offset_u32 (p0, z0, x0),
		     z0_res = svldff1sb_gather_offset_u32 (p0, z0, x0))

/*
** ldff1sb_gather_m1_u32_offset:
**	mov	(x[0-9]+), #?-1
**	ldff1sb	z0\.s, p0/z, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1sb_gather_m1_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svldff1sb_gather_u32base_offset_u32 (p0, z0, -1),
		     z0_res = svldff1sb_gather_offset_u32 (p0, z0, -1))

/*
** ldff1sb_gather_0_u32_offset:
**	ldff1sb	z0\.s, p0/z, \[z0\.s\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1sb_gather_0_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svldff1sb_gather_u32base_offset_u32 (p0, z0, 0),
		     z0_res = svldff1sb_gather_offset_u32 (p0, z0, 0))

/*
** ldff1sb_gather_5_u32_offset:
**	ldff1sb	z0\.s, p0/z, \[z0\.s, #5\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1sb_gather_5_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svldff1sb_gather_u32base_offset_u32 (p0, z0, 5),
		     z0_res = svldff1sb_gather_offset_u32 (p0, z0, 5))

/*
** ldff1sb_gather_31_u32_offset:
**	ldff1sb	z0\.s, p0/z, \[z0\.s, #31\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1sb_gather_31_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svldff1sb_gather_u32base_offset_u32 (p0, z0, 31),
		     z0_res = svldff1sb_gather_offset_u32 (p0, z0, 31))

/*
** ldff1sb_gather_32_u32_offset:
**	mov	(x[0-9]+), #?32
**	ldff1sb	z0\.s, p0/z, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1sb_gather_32_u32_offset, svuint32_t, svuint32_t,
		     z0_res = svldff1sb_gather_u32base_offset_u32 (p0, z0, 32),
		     z0_res = svldff1sb_gather_offset_u32 (p0, z0, 32))

/*
** ldff1sb_gather_x0_u32_s32offset:
**	ldff1sb	z0\.s, p0/z, \[x0, z0\.s, sxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1sb_gather_x0_u32_s32offset, svuint32_t, int8_t, svint32_t,
		     z0_res = svldff1sb_gather_s32offset_u32 (p0, x0, z0),
		     z0_res = svldff1sb_gather_offset_u32 (p0, x0, z0))

/*
** ldff1sb_gather_tied1_u32_s32offset:
**	ldff1sb	z0\.s, p0/z, \[x0, z0\.s, sxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1sb_gather_tied1_u32_s32offset, svuint32_t, int8_t, svint32_t,
		     z0_res = svldff1sb_gather_s32offset_u32 (p0, x0, z0),
		     z0_res = svldff1sb_gather_offset_u32 (p0, x0, z0))

/*
** ldff1sb_gather_untied_u32_s32offset:
**	ldff1sb	z0\.s, p0/z, \[x0, z1\.s, sxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1sb_gather_untied_u32_s32offset, svuint32_t, int8_t, svint32_t,
		     z0_res = svldff1sb_gather_s32offset_u32 (p0, x0, z1),
		     z0_res = svldff1sb_gather_offset_u32 (p0, x0, z1))

/*
** ldff1sb_gather_x0_u32_u32offset:
**	ldff1sb	z0\.s, p0/z, \[x0, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1sb_gather_x0_u32_u32offset, svuint32_t, int8_t, svuint32_t,
		     z0_res = svldff1sb_gather_u32offset_u32 (p0, x0, z0),
		     z0_res = svldff1sb_gather_offset_u32 (p0, x0, z0))

/*
** ldff1sb_gather_tied1_u32_u32offset:
**	ldff1sb	z0\.s, p0/z, \[x0, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1sb_gather_tied1_u32_u32offset, svuint32_t, int8_t, svuint32_t,
		     z0_res = svldff1sb_gather_u32offset_u32 (p0, x0, z0),
		     z0_res = svldff1sb_gather_offset_u32 (p0, x0, z0))

/*
** ldff1sb_gather_untied_u32_u32offset:
**	ldff1sb	z0\.s, p0/z, \[x0, z1\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1sb_gather_untied_u32_u32offset, svuint32_t, int8_t, svuint32_t,
		     z0_res = svldff1sb_gather_u32offset_u32 (p0, x0, z1),
		     z0_res = svldff1sb_gather_offset_u32 (p0, x0, z1))
