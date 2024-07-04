/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ld1_gather_u64_tied1:
**	ld1d	z0\.d, p0/z, \[z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_u64_tied1, svuint64_t, svuint64_t,
		     z0_res = svld1_gather_u64base_u64 (p0, z0),
		     z0_res = svld1_gather_u64 (p0, z0))

/*
** ld1_gather_u64_untied:
**	ld1d	z0\.d, p0/z, \[z1\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_u64_untied, svuint64_t, svuint64_t,
		     z0_res = svld1_gather_u64base_u64 (p0, z1),
		     z0_res = svld1_gather_u64 (p0, z1))

/*
** ld1_gather_x0_u64_offset:
**	ld1d	z0\.d, p0/z, \[x0, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_x0_u64_offset, svuint64_t, svuint64_t,
		     z0_res = svld1_gather_u64base_offset_u64 (p0, z0, x0),
		     z0_res = svld1_gather_offset_u64 (p0, z0, x0))

/*
** ld1_gather_m8_u64_offset:
**	mov	(x[0-9]+), #?-8
**	ld1d	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_m8_u64_offset, svuint64_t, svuint64_t,
		     z0_res = svld1_gather_u64base_offset_u64 (p0, z0, -8),
		     z0_res = svld1_gather_offset_u64 (p0, z0, -8))

/*
** ld1_gather_0_u64_offset:
**	ld1d	z0\.d, p0/z, \[z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_0_u64_offset, svuint64_t, svuint64_t,
		     z0_res = svld1_gather_u64base_offset_u64 (p0, z0, 0),
		     z0_res = svld1_gather_offset_u64 (p0, z0, 0))

/*
** ld1_gather_9_u64_offset:
**	mov	(x[0-9]+), #?9
**	ld1d	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_9_u64_offset, svuint64_t, svuint64_t,
		     z0_res = svld1_gather_u64base_offset_u64 (p0, z0, 9),
		     z0_res = svld1_gather_offset_u64 (p0, z0, 9))

/*
** ld1_gather_10_u64_offset:
**	mov	(x[0-9]+), #?10
**	ld1d	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_10_u64_offset, svuint64_t, svuint64_t,
		     z0_res = svld1_gather_u64base_offset_u64 (p0, z0, 10),
		     z0_res = svld1_gather_offset_u64 (p0, z0, 10))

/*
** ld1_gather_11_u64_offset:
**	mov	(x[0-9]+), #?11
**	ld1d	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_11_u64_offset, svuint64_t, svuint64_t,
		     z0_res = svld1_gather_u64base_offset_u64 (p0, z0, 11),
		     z0_res = svld1_gather_offset_u64 (p0, z0, 11))

/*
** ld1_gather_12_u64_offset:
**	mov	(x[0-9]+), #?12
**	ld1d	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_12_u64_offset, svuint64_t, svuint64_t,
		     z0_res = svld1_gather_u64base_offset_u64 (p0, z0, 12),
		     z0_res = svld1_gather_offset_u64 (p0, z0, 12))

/*
** ld1_gather_13_u64_offset:
**	mov	(x[0-9]+), #?13
**	ld1d	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_13_u64_offset, svuint64_t, svuint64_t,
		     z0_res = svld1_gather_u64base_offset_u64 (p0, z0, 13),
		     z0_res = svld1_gather_offset_u64 (p0, z0, 13))

/*
** ld1_gather_14_u64_offset:
**	mov	(x[0-9]+), #?14
**	ld1d	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_14_u64_offset, svuint64_t, svuint64_t,
		     z0_res = svld1_gather_u64base_offset_u64 (p0, z0, 14),
		     z0_res = svld1_gather_offset_u64 (p0, z0, 14))

/*
** ld1_gather_15_u64_offset:
**	mov	(x[0-9]+), #?15
**	ld1d	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_15_u64_offset, svuint64_t, svuint64_t,
		     z0_res = svld1_gather_u64base_offset_u64 (p0, z0, 15),
		     z0_res = svld1_gather_offset_u64 (p0, z0, 15))

/*
** ld1_gather_16_u64_offset:
**	ld1d	z0\.d, p0/z, \[z0\.d, #16\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_16_u64_offset, svuint64_t, svuint64_t,
		     z0_res = svld1_gather_u64base_offset_u64 (p0, z0, 16),
		     z0_res = svld1_gather_offset_u64 (p0, z0, 16))

/*
** ld1_gather_248_u64_offset:
**	ld1d	z0\.d, p0/z, \[z0\.d, #248\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_248_u64_offset, svuint64_t, svuint64_t,
		     z0_res = svld1_gather_u64base_offset_u64 (p0, z0, 248),
		     z0_res = svld1_gather_offset_u64 (p0, z0, 248))

/*
** ld1_gather_256_u64_offset:
**	mov	(x[0-9]+), #?256
**	ld1d	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_256_u64_offset, svuint64_t, svuint64_t,
		     z0_res = svld1_gather_u64base_offset_u64 (p0, z0, 256),
		     z0_res = svld1_gather_offset_u64 (p0, z0, 256))

/*
** ld1_gather_x0_u64_index:
**	lsl	(x[0-9]+), x0, #?3
**	ld1d	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_x0_u64_index, svuint64_t, svuint64_t,
		     z0_res = svld1_gather_u64base_index_u64 (p0, z0, x0),
		     z0_res = svld1_gather_index_u64 (p0, z0, x0))

/*
** ld1_gather_m1_u64_index:
**	mov	(x[0-9]+), #?-8
**	ld1d	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_m1_u64_index, svuint64_t, svuint64_t,
		     z0_res = svld1_gather_u64base_index_u64 (p0, z0, -1),
		     z0_res = svld1_gather_index_u64 (p0, z0, -1))

/*
** ld1_gather_0_u64_index:
**	ld1d	z0\.d, p0/z, \[z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_0_u64_index, svuint64_t, svuint64_t,
		     z0_res = svld1_gather_u64base_index_u64 (p0, z0, 0),
		     z0_res = svld1_gather_index_u64 (p0, z0, 0))

/*
** ld1_gather_5_u64_index:
**	ld1d	z0\.d, p0/z, \[z0\.d, #40\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_5_u64_index, svuint64_t, svuint64_t,
		     z0_res = svld1_gather_u64base_index_u64 (p0, z0, 5),
		     z0_res = svld1_gather_index_u64 (p0, z0, 5))

/*
** ld1_gather_31_u64_index:
**	ld1d	z0\.d, p0/z, \[z0\.d, #248\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_31_u64_index, svuint64_t, svuint64_t,
		     z0_res = svld1_gather_u64base_index_u64 (p0, z0, 31),
		     z0_res = svld1_gather_index_u64 (p0, z0, 31))

/*
** ld1_gather_32_u64_index:
**	mov	(x[0-9]+), #?256
**	ld1d	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ld1_gather_32_u64_index, svuint64_t, svuint64_t,
		     z0_res = svld1_gather_u64base_index_u64 (p0, z0, 32),
		     z0_res = svld1_gather_index_u64 (p0, z0, 32))

/*
** ld1_gather_x0_u64_s64offset:
**	ld1d	z0\.d, p0/z, \[x0, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_x0_u64_s64offset, svuint64_t, uint64_t, svint64_t,
		     z0_res = svld1_gather_s64offset_u64 (p0, x0, z0),
		     z0_res = svld1_gather_offset (p0, x0, z0))

/*
** ld1_gather_tied1_u64_s64offset:
**	ld1d	z0\.d, p0/z, \[x0, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_tied1_u64_s64offset, svuint64_t, uint64_t, svint64_t,
		     z0_res = svld1_gather_s64offset_u64 (p0, x0, z0),
		     z0_res = svld1_gather_offset (p0, x0, z0))

/*
** ld1_gather_untied_u64_s64offset:
**	ld1d	z0\.d, p0/z, \[x0, z1\.d\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_untied_u64_s64offset, svuint64_t, uint64_t, svint64_t,
		     z0_res = svld1_gather_s64offset_u64 (p0, x0, z1),
		     z0_res = svld1_gather_offset (p0, x0, z1))

/*
** ld1_gather_ext_u64_s64offset:
**	ld1d	z0\.d, p0/z, \[x0, z1\.d, sxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_ext_u64_s64offset, svuint64_t, uint64_t, svint64_t,
		     z0_res = svld1_gather_s64offset_u64 (p0, x0, svextw_s64_x (p0, z1)),
		     z0_res = svld1_gather_offset (p0, x0, svextw_x (p0, z1)))

/*
** ld1_gather_x0_u64_u64offset:
**	ld1d	z0\.d, p0/z, \[x0, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_x0_u64_u64offset, svuint64_t, uint64_t, svuint64_t,
		     z0_res = svld1_gather_u64offset_u64 (p0, x0, z0),
		     z0_res = svld1_gather_offset (p0, x0, z0))

/*
** ld1_gather_tied1_u64_u64offset:
**	ld1d	z0\.d, p0/z, \[x0, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_tied1_u64_u64offset, svuint64_t, uint64_t, svuint64_t,
		     z0_res = svld1_gather_u64offset_u64 (p0, x0, z0),
		     z0_res = svld1_gather_offset (p0, x0, z0))

/*
** ld1_gather_untied_u64_u64offset:
**	ld1d	z0\.d, p0/z, \[x0, z1\.d\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_untied_u64_u64offset, svuint64_t, uint64_t, svuint64_t,
		     z0_res = svld1_gather_u64offset_u64 (p0, x0, z1),
		     z0_res = svld1_gather_offset (p0, x0, z1))

/*
** ld1_gather_ext_u64_u64offset:
**	ld1d	z0\.d, p0/z, \[x0, z1\.d, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_ext_u64_u64offset, svuint64_t, uint64_t, svuint64_t,
		     z0_res = svld1_gather_u64offset_u64 (p0, x0, svextw_u64_x (p0, z1)),
		     z0_res = svld1_gather_offset (p0, x0, svextw_x (p0, z1)))

/*
** ld1_gather_x0_u64_s64index:
**	ld1d	z0\.d, p0/z, \[x0, z0\.d, lsl 3\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_x0_u64_s64index, svuint64_t, uint64_t, svint64_t,
		     z0_res = svld1_gather_s64index_u64 (p0, x0, z0),
		     z0_res = svld1_gather_index (p0, x0, z0))

/*
** ld1_gather_tied1_u64_s64index:
**	ld1d	z0\.d, p0/z, \[x0, z0\.d, lsl 3\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_tied1_u64_s64index, svuint64_t, uint64_t, svint64_t,
		     z0_res = svld1_gather_s64index_u64 (p0, x0, z0),
		     z0_res = svld1_gather_index (p0, x0, z0))

/*
** ld1_gather_untied_u64_s64index:
**	ld1d	z0\.d, p0/z, \[x0, z1\.d, lsl 3\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_untied_u64_s64index, svuint64_t, uint64_t, svint64_t,
		     z0_res = svld1_gather_s64index_u64 (p0, x0, z1),
		     z0_res = svld1_gather_index (p0, x0, z1))

/*
** ld1_gather_ext_u64_s64index:
**	ld1d	z0\.d, p0/z, \[x0, z1\.d, sxtw 3\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_ext_u64_s64index, svuint64_t, uint64_t, svint64_t,
		     z0_res = svld1_gather_s64index_u64 (p0, x0, svextw_s64_x (p0, z1)),
		     z0_res = svld1_gather_index (p0, x0, svextw_x (p0, z1)))

/*
** ld1_gather_x0_u64_u64index:
**	ld1d	z0\.d, p0/z, \[x0, z0\.d, lsl 3\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_x0_u64_u64index, svuint64_t, uint64_t, svuint64_t,
		     z0_res = svld1_gather_u64index_u64 (p0, x0, z0),
		     z0_res = svld1_gather_index (p0, x0, z0))

/*
** ld1_gather_tied1_u64_u64index:
**	ld1d	z0\.d, p0/z, \[x0, z0\.d, lsl 3\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_tied1_u64_u64index, svuint64_t, uint64_t, svuint64_t,
		     z0_res = svld1_gather_u64index_u64 (p0, x0, z0),
		     z0_res = svld1_gather_index (p0, x0, z0))

/*
** ld1_gather_untied_u64_u64index:
**	ld1d	z0\.d, p0/z, \[x0, z1\.d, lsl 3\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_untied_u64_u64index, svuint64_t, uint64_t, svuint64_t,
		     z0_res = svld1_gather_u64index_u64 (p0, x0, z1),
		     z0_res = svld1_gather_index (p0, x0, z1))

/*
** ld1_gather_ext_u64_u64index:
**	ld1d	z0\.d, p0/z, \[x0, z1\.d, uxtw 3\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ld1_gather_ext_u64_u64index, svuint64_t, uint64_t, svuint64_t,
		     z0_res = svld1_gather_u64index_u64 (p0, x0, svextw_u64_x (p0, z1)),
		     z0_res = svld1_gather_index (p0, x0, svextw_x (p0, z1)))
