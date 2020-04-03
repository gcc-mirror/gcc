/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ldff1sh_gather_u64_tied1:
**	ldff1sh	z0\.d, p0/z, \[z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1sh_gather_u64_tied1, svuint64_t, svuint64_t,
		     z0_res = svldff1sh_gather_u64base_u64 (p0, z0),
		     z0_res = svldff1sh_gather_u64 (p0, z0))

/*
** ldff1sh_gather_u64_untied:
**	ldff1sh	z0\.d, p0/z, \[z1\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1sh_gather_u64_untied, svuint64_t, svuint64_t,
		     z0_res = svldff1sh_gather_u64base_u64 (p0, z1),
		     z0_res = svldff1sh_gather_u64 (p0, z1))

/*
** ldff1sh_gather_x0_u64_offset:
**	ldff1sh	z0\.d, p0/z, \[x0, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1sh_gather_x0_u64_offset, svuint64_t, svuint64_t,
		     z0_res = svldff1sh_gather_u64base_offset_u64 (p0, z0, x0),
		     z0_res = svldff1sh_gather_offset_u64 (p0, z0, x0))

/*
** ldff1sh_gather_m2_u64_offset:
**	mov	(x[0-9]+), #?-2
**	ldff1sh	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1sh_gather_m2_u64_offset, svuint64_t, svuint64_t,
		     z0_res = svldff1sh_gather_u64base_offset_u64 (p0, z0, -2),
		     z0_res = svldff1sh_gather_offset_u64 (p0, z0, -2))

/*
** ldff1sh_gather_0_u64_offset:
**	ldff1sh	z0\.d, p0/z, \[z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1sh_gather_0_u64_offset, svuint64_t, svuint64_t,
		     z0_res = svldff1sh_gather_u64base_offset_u64 (p0, z0, 0),
		     z0_res = svldff1sh_gather_offset_u64 (p0, z0, 0))

/*
** ldff1sh_gather_5_u64_offset:
**	mov	(x[0-9]+), #?5
**	ldff1sh	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1sh_gather_5_u64_offset, svuint64_t, svuint64_t,
		     z0_res = svldff1sh_gather_u64base_offset_u64 (p0, z0, 5),
		     z0_res = svldff1sh_gather_offset_u64 (p0, z0, 5))

/*
** ldff1sh_gather_6_u64_offset:
**	ldff1sh	z0\.d, p0/z, \[z0\.d, #6\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1sh_gather_6_u64_offset, svuint64_t, svuint64_t,
		     z0_res = svldff1sh_gather_u64base_offset_u64 (p0, z0, 6),
		     z0_res = svldff1sh_gather_offset_u64 (p0, z0, 6))

/*
** ldff1sh_gather_62_u64_offset:
**	ldff1sh	z0\.d, p0/z, \[z0\.d, #62\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1sh_gather_62_u64_offset, svuint64_t, svuint64_t,
		     z0_res = svldff1sh_gather_u64base_offset_u64 (p0, z0, 62),
		     z0_res = svldff1sh_gather_offset_u64 (p0, z0, 62))

/*
** ldff1sh_gather_64_u64_offset:
**	mov	(x[0-9]+), #?64
**	ldff1sh	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1sh_gather_64_u64_offset, svuint64_t, svuint64_t,
		     z0_res = svldff1sh_gather_u64base_offset_u64 (p0, z0, 64),
		     z0_res = svldff1sh_gather_offset_u64 (p0, z0, 64))

/*
** ldff1sh_gather_x0_u64_index:
**	lsl	(x[0-9]+), x0, #?1
**	ldff1sh	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1sh_gather_x0_u64_index, svuint64_t, svuint64_t,
		     z0_res = svldff1sh_gather_u64base_index_u64 (p0, z0, x0),
		     z0_res = svldff1sh_gather_index_u64 (p0, z0, x0))

/*
** ldff1sh_gather_m1_u64_index:
**	mov	(x[0-9]+), #?-2
**	ldff1sh	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1sh_gather_m1_u64_index, svuint64_t, svuint64_t,
		     z0_res = svldff1sh_gather_u64base_index_u64 (p0, z0, -1),
		     z0_res = svldff1sh_gather_index_u64 (p0, z0, -1))

/*
** ldff1sh_gather_0_u64_index:
**	ldff1sh	z0\.d, p0/z, \[z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1sh_gather_0_u64_index, svuint64_t, svuint64_t,
		     z0_res = svldff1sh_gather_u64base_index_u64 (p0, z0, 0),
		     z0_res = svldff1sh_gather_index_u64 (p0, z0, 0))

/*
** ldff1sh_gather_5_u64_index:
**	ldff1sh	z0\.d, p0/z, \[z0\.d, #10\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1sh_gather_5_u64_index, svuint64_t, svuint64_t,
		     z0_res = svldff1sh_gather_u64base_index_u64 (p0, z0, 5),
		     z0_res = svldff1sh_gather_index_u64 (p0, z0, 5))

/*
** ldff1sh_gather_31_u64_index:
**	ldff1sh	z0\.d, p0/z, \[z0\.d, #62\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1sh_gather_31_u64_index, svuint64_t, svuint64_t,
		     z0_res = svldff1sh_gather_u64base_index_u64 (p0, z0, 31),
		     z0_res = svldff1sh_gather_index_u64 (p0, z0, 31))

/*
** ldff1sh_gather_32_u64_index:
**	mov	(x[0-9]+), #?64
**	ldff1sh	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1sh_gather_32_u64_index, svuint64_t, svuint64_t,
		     z0_res = svldff1sh_gather_u64base_index_u64 (p0, z0, 32),
		     z0_res = svldff1sh_gather_index_u64 (p0, z0, 32))

/*
** ldff1sh_gather_x0_u64_s64offset:
**	ldff1sh	z0\.d, p0/z, \[x0, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1sh_gather_x0_u64_s64offset, svuint64_t, int16_t, svint64_t,
		     z0_res = svldff1sh_gather_s64offset_u64 (p0, x0, z0),
		     z0_res = svldff1sh_gather_offset_u64 (p0, x0, z0))

/*
** ldff1sh_gather_tied1_u64_s64offset:
**	ldff1sh	z0\.d, p0/z, \[x0, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1sh_gather_tied1_u64_s64offset, svuint64_t, int16_t, svint64_t,
		     z0_res = svldff1sh_gather_s64offset_u64 (p0, x0, z0),
		     z0_res = svldff1sh_gather_offset_u64 (p0, x0, z0))

/*
** ldff1sh_gather_untied_u64_s64offset:
**	ldff1sh	z0\.d, p0/z, \[x0, z1\.d\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1sh_gather_untied_u64_s64offset, svuint64_t, int16_t, svint64_t,
		     z0_res = svldff1sh_gather_s64offset_u64 (p0, x0, z1),
		     z0_res = svldff1sh_gather_offset_u64 (p0, x0, z1))

/*
** ldff1sh_gather_ext_u64_s64offset:
**	ldff1sh	z0\.d, p0/z, \[x0, z1\.d, sxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1sh_gather_ext_u64_s64offset, svuint64_t, int16_t, svint64_t,
		     z0_res = svldff1sh_gather_s64offset_u64 (p0, x0, svextw_s64_x (p0, z1)),
		     z0_res = svldff1sh_gather_offset_u64 (p0, x0, svextw_x (p0, z1)))

/*
** ldff1sh_gather_x0_u64_u64offset:
**	ldff1sh	z0\.d, p0/z, \[x0, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1sh_gather_x0_u64_u64offset, svuint64_t, int16_t, svuint64_t,
		     z0_res = svldff1sh_gather_u64offset_u64 (p0, x0, z0),
		     z0_res = svldff1sh_gather_offset_u64 (p0, x0, z0))

/*
** ldff1sh_gather_tied1_u64_u64offset:
**	ldff1sh	z0\.d, p0/z, \[x0, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1sh_gather_tied1_u64_u64offset, svuint64_t, int16_t, svuint64_t,
		     z0_res = svldff1sh_gather_u64offset_u64 (p0, x0, z0),
		     z0_res = svldff1sh_gather_offset_u64 (p0, x0, z0))

/*
** ldff1sh_gather_untied_u64_u64offset:
**	ldff1sh	z0\.d, p0/z, \[x0, z1\.d\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1sh_gather_untied_u64_u64offset, svuint64_t, int16_t, svuint64_t,
		     z0_res = svldff1sh_gather_u64offset_u64 (p0, x0, z1),
		     z0_res = svldff1sh_gather_offset_u64 (p0, x0, z1))

/*
** ldff1sh_gather_ext_u64_u64offset:
**	ldff1sh	z0\.d, p0/z, \[x0, z1\.d, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1sh_gather_ext_u64_u64offset, svuint64_t, int16_t, svuint64_t,
		     z0_res = svldff1sh_gather_u64offset_u64 (p0, x0, svextw_u64_x (p0, z1)),
		     z0_res = svldff1sh_gather_offset_u64 (p0, x0, svextw_x (p0, z1)))

/*
** ldff1sh_gather_x0_u64_s64index:
**	ldff1sh	z0\.d, p0/z, \[x0, z0\.d, lsl 1\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1sh_gather_x0_u64_s64index, svuint64_t, int16_t, svint64_t,
		     z0_res = svldff1sh_gather_s64index_u64 (p0, x0, z0),
		     z0_res = svldff1sh_gather_index_u64 (p0, x0, z0))

/*
** ldff1sh_gather_tied1_u64_s64index:
**	ldff1sh	z0\.d, p0/z, \[x0, z0\.d, lsl 1\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1sh_gather_tied1_u64_s64index, svuint64_t, int16_t, svint64_t,
		     z0_res = svldff1sh_gather_s64index_u64 (p0, x0, z0),
		     z0_res = svldff1sh_gather_index_u64 (p0, x0, z0))

/*
** ldff1sh_gather_untied_u64_s64index:
**	ldff1sh	z0\.d, p0/z, \[x0, z1\.d, lsl 1\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1sh_gather_untied_u64_s64index, svuint64_t, int16_t, svint64_t,
		     z0_res = svldff1sh_gather_s64index_u64 (p0, x0, z1),
		     z0_res = svldff1sh_gather_index_u64 (p0, x0, z1))

/*
** ldff1sh_gather_ext_u64_s64index:
**	ldff1sh	z0\.d, p0/z, \[x0, z1\.d, sxtw 1\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1sh_gather_ext_u64_s64index, svuint64_t, int16_t, svint64_t,
		     z0_res = svldff1sh_gather_s64index_u64 (p0, x0, svextw_s64_x (p0, z1)),
		     z0_res = svldff1sh_gather_index_u64 (p0, x0, svextw_x (p0, z1)))

/*
** ldff1sh_gather_x0_u64_u64index:
**	ldff1sh	z0\.d, p0/z, \[x0, z0\.d, lsl 1\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1sh_gather_x0_u64_u64index, svuint64_t, int16_t, svuint64_t,
		     z0_res = svldff1sh_gather_u64index_u64 (p0, x0, z0),
		     z0_res = svldff1sh_gather_index_u64 (p0, x0, z0))

/*
** ldff1sh_gather_tied1_u64_u64index:
**	ldff1sh	z0\.d, p0/z, \[x0, z0\.d, lsl 1\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1sh_gather_tied1_u64_u64index, svuint64_t, int16_t, svuint64_t,
		     z0_res = svldff1sh_gather_u64index_u64 (p0, x0, z0),
		     z0_res = svldff1sh_gather_index_u64 (p0, x0, z0))

/*
** ldff1sh_gather_untied_u64_u64index:
**	ldff1sh	z0\.d, p0/z, \[x0, z1\.d, lsl 1\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1sh_gather_untied_u64_u64index, svuint64_t, int16_t, svuint64_t,
		     z0_res = svldff1sh_gather_u64index_u64 (p0, x0, z1),
		     z0_res = svldff1sh_gather_index_u64 (p0, x0, z1))

/*
** ldff1sh_gather_ext_u64_u64index:
**	ldff1sh	z0\.d, p0/z, \[x0, z1\.d, uxtw 1\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1sh_gather_ext_u64_u64index, svuint64_t, int16_t, svuint64_t,
		     z0_res = svldff1sh_gather_u64index_u64 (p0, x0, svextw_u64_x (p0, z1)),
		     z0_res = svldff1sh_gather_index_u64 (p0, x0, svextw_x (p0, z1)))
