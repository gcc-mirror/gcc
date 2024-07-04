/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ldff1_gather_f64_tied1:
**	ldff1d	z0\.d, p0/z, \[z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1_gather_f64_tied1, svfloat64_t, svuint64_t,
		     z0_res = svldff1_gather_u64base_f64 (p0, z0),
		     z0_res = svldff1_gather_f64 (p0, z0))

/*
** ldff1_gather_f64_untied:
**	ldff1d	z0\.d, p0/z, \[z1\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1_gather_f64_untied, svfloat64_t, svuint64_t,
		     z0_res = svldff1_gather_u64base_f64 (p0, z1),
		     z0_res = svldff1_gather_f64 (p0, z1))

/*
** ldff1_gather_x0_f64_offset:
**	ldff1d	z0\.d, p0/z, \[x0, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1_gather_x0_f64_offset, svfloat64_t, svuint64_t,
		     z0_res = svldff1_gather_u64base_offset_f64 (p0, z0, x0),
		     z0_res = svldff1_gather_offset_f64 (p0, z0, x0))

/*
** ldff1_gather_m8_f64_offset:
**	mov	(x[0-9]+), #?-8
**	ldff1d	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1_gather_m8_f64_offset, svfloat64_t, svuint64_t,
		     z0_res = svldff1_gather_u64base_offset_f64 (p0, z0, -8),
		     z0_res = svldff1_gather_offset_f64 (p0, z0, -8))

/*
** ldff1_gather_0_f64_offset:
**	ldff1d	z0\.d, p0/z, \[z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1_gather_0_f64_offset, svfloat64_t, svuint64_t,
		     z0_res = svldff1_gather_u64base_offset_f64 (p0, z0, 0),
		     z0_res = svldff1_gather_offset_f64 (p0, z0, 0))

/*
** ldff1_gather_9_f64_offset:
**	mov	(x[0-9]+), #?9
**	ldff1d	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1_gather_9_f64_offset, svfloat64_t, svuint64_t,
		     z0_res = svldff1_gather_u64base_offset_f64 (p0, z0, 9),
		     z0_res = svldff1_gather_offset_f64 (p0, z0, 9))

/*
** ldff1_gather_10_f64_offset:
**	mov	(x[0-9]+), #?10
**	ldff1d	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1_gather_10_f64_offset, svfloat64_t, svuint64_t,
		     z0_res = svldff1_gather_u64base_offset_f64 (p0, z0, 10),
		     z0_res = svldff1_gather_offset_f64 (p0, z0, 10))

/*
** ldff1_gather_11_f64_offset:
**	mov	(x[0-9]+), #?11
**	ldff1d	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1_gather_11_f64_offset, svfloat64_t, svuint64_t,
		     z0_res = svldff1_gather_u64base_offset_f64 (p0, z0, 11),
		     z0_res = svldff1_gather_offset_f64 (p0, z0, 11))

/*
** ldff1_gather_12_f64_offset:
**	mov	(x[0-9]+), #?12
**	ldff1d	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1_gather_12_f64_offset, svfloat64_t, svuint64_t,
		     z0_res = svldff1_gather_u64base_offset_f64 (p0, z0, 12),
		     z0_res = svldff1_gather_offset_f64 (p0, z0, 12))

/*
** ldff1_gather_13_f64_offset:
**	mov	(x[0-9]+), #?13
**	ldff1d	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1_gather_13_f64_offset, svfloat64_t, svuint64_t,
		     z0_res = svldff1_gather_u64base_offset_f64 (p0, z0, 13),
		     z0_res = svldff1_gather_offset_f64 (p0, z0, 13))

/*
** ldff1_gather_14_f64_offset:
**	mov	(x[0-9]+), #?14
**	ldff1d	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1_gather_14_f64_offset, svfloat64_t, svuint64_t,
		     z0_res = svldff1_gather_u64base_offset_f64 (p0, z0, 14),
		     z0_res = svldff1_gather_offset_f64 (p0, z0, 14))

/*
** ldff1_gather_15_f64_offset:
**	mov	(x[0-9]+), #?15
**	ldff1d	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1_gather_15_f64_offset, svfloat64_t, svuint64_t,
		     z0_res = svldff1_gather_u64base_offset_f64 (p0, z0, 15),
		     z0_res = svldff1_gather_offset_f64 (p0, z0, 15))

/*
** ldff1_gather_16_f64_offset:
**	ldff1d	z0\.d, p0/z, \[z0\.d, #16\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1_gather_16_f64_offset, svfloat64_t, svuint64_t,
		     z0_res = svldff1_gather_u64base_offset_f64 (p0, z0, 16),
		     z0_res = svldff1_gather_offset_f64 (p0, z0, 16))

/*
** ldff1_gather_248_f64_offset:
**	ldff1d	z0\.d, p0/z, \[z0\.d, #248\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1_gather_248_f64_offset, svfloat64_t, svuint64_t,
		     z0_res = svldff1_gather_u64base_offset_f64 (p0, z0, 248),
		     z0_res = svldff1_gather_offset_f64 (p0, z0, 248))

/*
** ldff1_gather_256_f64_offset:
**	mov	(x[0-9]+), #?256
**	ldff1d	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1_gather_256_f64_offset, svfloat64_t, svuint64_t,
		     z0_res = svldff1_gather_u64base_offset_f64 (p0, z0, 256),
		     z0_res = svldff1_gather_offset_f64 (p0, z0, 256))

/*
** ldff1_gather_x0_f64_index:
**	lsl	(x[0-9]+), x0, #?3
**	ldff1d	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1_gather_x0_f64_index, svfloat64_t, svuint64_t,
		     z0_res = svldff1_gather_u64base_index_f64 (p0, z0, x0),
		     z0_res = svldff1_gather_index_f64 (p0, z0, x0))

/*
** ldff1_gather_m1_f64_index:
**	mov	(x[0-9]+), #?-8
**	ldff1d	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1_gather_m1_f64_index, svfloat64_t, svuint64_t,
		     z0_res = svldff1_gather_u64base_index_f64 (p0, z0, -1),
		     z0_res = svldff1_gather_index_f64 (p0, z0, -1))

/*
** ldff1_gather_0_f64_index:
**	ldff1d	z0\.d, p0/z, \[z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1_gather_0_f64_index, svfloat64_t, svuint64_t,
		     z0_res = svldff1_gather_u64base_index_f64 (p0, z0, 0),
		     z0_res = svldff1_gather_index_f64 (p0, z0, 0))

/*
** ldff1_gather_5_f64_index:
**	ldff1d	z0\.d, p0/z, \[z0\.d, #40\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1_gather_5_f64_index, svfloat64_t, svuint64_t,
		     z0_res = svldff1_gather_u64base_index_f64 (p0, z0, 5),
		     z0_res = svldff1_gather_index_f64 (p0, z0, 5))

/*
** ldff1_gather_31_f64_index:
**	ldff1d	z0\.d, p0/z, \[z0\.d, #248\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1_gather_31_f64_index, svfloat64_t, svuint64_t,
		     z0_res = svldff1_gather_u64base_index_f64 (p0, z0, 31),
		     z0_res = svldff1_gather_index_f64 (p0, z0, 31))

/*
** ldff1_gather_32_f64_index:
**	mov	(x[0-9]+), #?256
**	ldff1d	z0\.d, p0/z, \[\1, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1_gather_32_f64_index, svfloat64_t, svuint64_t,
		     z0_res = svldff1_gather_u64base_index_f64 (p0, z0, 32),
		     z0_res = svldff1_gather_index_f64 (p0, z0, 32))

/*
** ldff1_gather_x0_f64_s64offset:
**	ldff1d	z0\.d, p0/z, \[x0, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1_gather_x0_f64_s64offset, svfloat64_t, float64_t, svint64_t,
		     z0_res = svldff1_gather_s64offset_f64 (p0, x0, z0),
		     z0_res = svldff1_gather_offset (p0, x0, z0))

/*
** ldff1_gather_tied1_f64_s64offset:
**	ldff1d	z0\.d, p0/z, \[x0, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1_gather_tied1_f64_s64offset, svfloat64_t, float64_t, svint64_t,
		     z0_res = svldff1_gather_s64offset_f64 (p0, x0, z0),
		     z0_res = svldff1_gather_offset (p0, x0, z0))

/*
** ldff1_gather_untied_f64_s64offset:
**	ldff1d	z0\.d, p0/z, \[x0, z1\.d\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1_gather_untied_f64_s64offset, svfloat64_t, float64_t, svint64_t,
		     z0_res = svldff1_gather_s64offset_f64 (p0, x0, z1),
		     z0_res = svldff1_gather_offset (p0, x0, z1))

/*
** ldff1_gather_ext_f64_s64offset:
**	ldff1d	z0\.d, p0/z, \[x0, z1\.d, sxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1_gather_ext_f64_s64offset, svfloat64_t, float64_t, svint64_t,
		     z0_res = svldff1_gather_s64offset_f64 (p0, x0, svextw_s64_x (p0, z1)),
		     z0_res = svldff1_gather_offset (p0, x0, svextw_x (p0, z1)))

/*
** ldff1_gather_x0_f64_u64offset:
**	ldff1d	z0\.d, p0/z, \[x0, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1_gather_x0_f64_u64offset, svfloat64_t, float64_t, svuint64_t,
		     z0_res = svldff1_gather_u64offset_f64 (p0, x0, z0),
		     z0_res = svldff1_gather_offset (p0, x0, z0))

/*
** ldff1_gather_tied1_f64_u64offset:
**	ldff1d	z0\.d, p0/z, \[x0, z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1_gather_tied1_f64_u64offset, svfloat64_t, float64_t, svuint64_t,
		     z0_res = svldff1_gather_u64offset_f64 (p0, x0, z0),
		     z0_res = svldff1_gather_offset (p0, x0, z0))

/*
** ldff1_gather_untied_f64_u64offset:
**	ldff1d	z0\.d, p0/z, \[x0, z1\.d\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1_gather_untied_f64_u64offset, svfloat64_t, float64_t, svuint64_t,
		     z0_res = svldff1_gather_u64offset_f64 (p0, x0, z1),
		     z0_res = svldff1_gather_offset (p0, x0, z1))

/*
** ldff1_gather_ext_f64_u64offset:
**	ldff1d	z0\.d, p0/z, \[x0, z1\.d, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1_gather_ext_f64_u64offset, svfloat64_t, float64_t, svuint64_t,
		     z0_res = svldff1_gather_u64offset_f64 (p0, x0, svextw_u64_x (p0, z1)),
		     z0_res = svldff1_gather_offset (p0, x0, svextw_x (p0, z1)))

/*
** ldff1_gather_x0_f64_s64index:
**	ldff1d	z0\.d, p0/z, \[x0, z0\.d, lsl 3\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1_gather_x0_f64_s64index, svfloat64_t, float64_t, svint64_t,
		     z0_res = svldff1_gather_s64index_f64 (p0, x0, z0),
		     z0_res = svldff1_gather_index (p0, x0, z0))

/*
** ldff1_gather_tied1_f64_s64index:
**	ldff1d	z0\.d, p0/z, \[x0, z0\.d, lsl 3\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1_gather_tied1_f64_s64index, svfloat64_t, float64_t, svint64_t,
		     z0_res = svldff1_gather_s64index_f64 (p0, x0, z0),
		     z0_res = svldff1_gather_index (p0, x0, z0))

/*
** ldff1_gather_untied_f64_s64index:
**	ldff1d	z0\.d, p0/z, \[x0, z1\.d, lsl 3\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1_gather_untied_f64_s64index, svfloat64_t, float64_t, svint64_t,
		     z0_res = svldff1_gather_s64index_f64 (p0, x0, z1),
		     z0_res = svldff1_gather_index (p0, x0, z1))

/*
** ldff1_gather_ext_f64_s64index:
**	ldff1d	z0\.d, p0/z, \[x0, z1\.d, sxtw 3\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1_gather_ext_f64_s64index, svfloat64_t, float64_t, svint64_t,
		     z0_res = svldff1_gather_s64index_f64 (p0, x0, svextw_s64_x (p0, z1)),
		     z0_res = svldff1_gather_index (p0, x0, svextw_x (p0, z1)))

/*
** ldff1_gather_x0_f64_u64index:
**	ldff1d	z0\.d, p0/z, \[x0, z0\.d, lsl 3\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1_gather_x0_f64_u64index, svfloat64_t, float64_t, svuint64_t,
		     z0_res = svldff1_gather_u64index_f64 (p0, x0, z0),
		     z0_res = svldff1_gather_index (p0, x0, z0))

/*
** ldff1_gather_tied1_f64_u64index:
**	ldff1d	z0\.d, p0/z, \[x0, z0\.d, lsl 3\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1_gather_tied1_f64_u64index, svfloat64_t, float64_t, svuint64_t,
		     z0_res = svldff1_gather_u64index_f64 (p0, x0, z0),
		     z0_res = svldff1_gather_index (p0, x0, z0))

/*
** ldff1_gather_untied_f64_u64index:
**	ldff1d	z0\.d, p0/z, \[x0, z1\.d, lsl 3\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1_gather_untied_f64_u64index, svfloat64_t, float64_t, svuint64_t,
		     z0_res = svldff1_gather_u64index_f64 (p0, x0, z1),
		     z0_res = svldff1_gather_index (p0, x0, z1))

/*
** ldff1_gather_ext_f64_u64index:
**	ldff1d	z0\.d, p0/z, \[x0, z1\.d, uxtw 3\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1_gather_ext_f64_u64index, svfloat64_t, float64_t, svuint64_t,
		     z0_res = svldff1_gather_u64index_f64 (p0, x0, svextw_u64_x (p0, z1)),
		     z0_res = svldff1_gather_index (p0, x0, svextw_x (p0, z1)))
