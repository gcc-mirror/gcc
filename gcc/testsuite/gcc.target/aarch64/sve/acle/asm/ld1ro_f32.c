/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */
/* { dg-additional-options "-march=armv8.6-a+f64mm" } */
/* { dg-require-effective-target aarch64_asm_f64mm_ok }  */

#include "test_sve_acle.h"

/*
** ld1ro_f32_base:
**	ld1row	z0\.s, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1ro_f32_base, svfloat32_t, float32_t,
	   z0 = svld1ro_f32 (p0, x0),
	   z0 = svld1ro (p0, x0))

/*
** ld1ro_f32_index:
**	ld1row	z0\.s, p0/z, \[x0, x1, lsl 2\]
**	ret
*/
TEST_LOAD (ld1ro_f32_index, svfloat32_t, float32_t,
	   z0 = svld1ro_f32 (p0, x0 + x1),
	   z0 = svld1ro (p0, x0 + x1))

/*
** ld1ro_f32_1:
**	add	(x[0-9]+), x0, #?4
**	ld1row	z0\.s, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1ro_f32_1, svfloat32_t, float32_t,
	   z0 = svld1ro_f32 (p0, x0 + 1),
	   z0 = svld1ro (p0, x0 + 1))

/*
** ld1ro_f32_4:
**	add	(x[0-9]+), x0, #?16
**	ld1row	z0\.s, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1ro_f32_4, svfloat32_t, float32_t,
	   z0 = svld1ro_f32 (p0, x0 + 4),
	   z0 = svld1ro (p0, x0 + 4))

/*
** ld1ro_f32_64:
**	add	(x[0-9]+), x0, #?256
**	ld1row	z0\.s, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1ro_f32_64, svfloat32_t, float32_t,
	   z0 = svld1ro_f32 (p0, x0 + 64),
	   z0 = svld1ro (p0, x0 + 64))

/*
** ld1ro_f32_m1:
**	sub	(x[0-9]+), x0, #?4
**	ld1row	z0\.s, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1ro_f32_m1, svfloat32_t, float32_t,
	   z0 = svld1ro_f32 (p0, x0 - 1),
	   z0 = svld1ro (p0, x0 - 1))

/*
** ld1ro_f32_m4:
**	sub	(x[0-9]+), x0, #?16
**	ld1row	z0\.s, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1ro_f32_m4, svfloat32_t, float32_t,
	   z0 = svld1ro_f32 (p0, x0 - 4),
	   z0 = svld1ro (p0, x0 - 4))

/*
** ld1ro_f32_m72:
**	sub	(x[0-9]+), x0, #?288
**	ld1row	z0\.s, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1ro_f32_m72, svfloat32_t, float32_t,
	   z0 = svld1ro_f32 (p0, x0 - 72),
	   z0 = svld1ro (p0, x0 - 72))

/*
** ld1ro_f32_8:
**	ld1row	z0\.s, p0/z, \[x0, #?32\]
**	ret
*/
TEST_LOAD (ld1ro_f32_8, svfloat32_t, float32_t,
	   z0 = svld1ro_f32 (p0, x0 + 8),
	   z0 = svld1ro (p0, x0 + 8))

/*
** ld1ro_f32_56:
**	ld1row	z0\.s, p0/z, \[x0, #?224\]
**	ret
*/
TEST_LOAD (ld1ro_f32_56, svfloat32_t, float32_t,
	   z0 = svld1ro_f32 (p0, x0 + 56),
	   z0 = svld1ro (p0, x0 + 56))

/*
** ld1ro_f32_m8:
**	ld1row	z0\.s, p0/z, \[x0, #?-32\]
**	ret
*/
TEST_LOAD (ld1ro_f32_m8, svfloat32_t, float32_t,
	   z0 = svld1ro_f32 (p0, x0 - 8),
	   z0 = svld1ro (p0, x0 - 8))

/*
** ld1ro_f32_m64:
**	ld1row	z0\.s, p0/z, \[x0, #?-256\]
**	ret
*/
TEST_LOAD (ld1ro_f32_m64, svfloat32_t, float32_t,
	   z0 = svld1ro_f32 (p0, x0 - 64),
	   z0 = svld1ro (p0, x0 - 64))

