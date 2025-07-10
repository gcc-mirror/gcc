/* { dg-options "-O2 -msve-vector-bits=256" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_sve.h>

#pragma GCC target "+sve2p1"

typedef svint8_t fixed_int8_t __attribute__((arm_sve_vector_bits(256)));
typedef svfloat16_t fixed_float16_t __attribute__((arm_sve_vector_bits(256)));
typedef svuint32_t fixed_uint32_t __attribute__((arm_sve_vector_bits(256)));
typedef svfloat64_t fixed_float64_t __attribute__((arm_sve_vector_bits(256)));

/*
** f1:
**	extq	z0\.b, z0\.b, z1\.b, #8
**	ret
*/
[[gnu::noipa]] fixed_float64_t
f1 (fixed_float64_t z0, fixed_float64_t z1)
{
  return __builtin_shufflevector (z0, z1, 1, 4, 3, 6);
}

/*
** f2:
**	extq	z0\.b, z0\.b, z1\.b, #4
**	ret
*/
[[gnu::noipa]] fixed_uint32_t
f2 (fixed_uint32_t z0, fixed_uint32_t z1)
{
  return __builtin_shufflevector (z0, z1, 1, 2, 3, 8, 5, 6, 7, 12);
}

/*
** f3:
**	extq	z0\.b, z0\.b, z1\.b, #12
**	ret
*/
[[gnu::noipa]] fixed_uint32_t
f3 (fixed_uint32_t z0, fixed_uint32_t z1)
{
  return __builtin_shufflevector (z0, z1, 3, 8, 9, 10, 7, 12, 13, 14);
}

/*
** f4:
**	extq	z0\.b, z0\.b, z1\.b, #2
**	ret
*/
[[gnu::noipa]] fixed_float16_t
f4 (fixed_float16_t z0, fixed_float16_t z1)
{
  return __builtin_shufflevector (z0, z1,
				  1, 2, 3, 4, 5, 6, 7, 16,
				  9, 10, 11, 12, 13, 14, 15, 24);
}

/*
** f5:
**	extq	z0\.b, z0\.b, z1\.b, #10
**	ret
*/
[[gnu::noipa]] fixed_float16_t
f5 (fixed_float16_t z0, fixed_float16_t z1)
{
  return __builtin_shufflevector (z0, z1,
				  5, 6, 7, 16, 17, 18, 19, 20,
				  13, 14, 15, 24, 25, 26, 27, 28);
}

/*
** f6:
**	extq	z0\.b, z0\.b, z1\.b, #14
**	ret
*/
[[gnu::noipa]] fixed_float16_t
f6 (fixed_float16_t z0, fixed_float16_t z1)
{
  return __builtin_shufflevector (z0, z1,
				  7, 16, 17, 18, 19, 20, 21, 22,
				  15, 24, 25, 26, 27, 28, 29, 30);
}

/*
** f7:
**	extq	z0\.b, z0\.b, z1\.b, #1
**	ret
*/
[[gnu::noipa]] fixed_int8_t
f7 (fixed_int8_t z0, fixed_int8_t z1)
{
  return __builtin_shufflevector (z0, z1,
				  1, 2, 3, 4, 5, 6, 7, 8,
				  9, 10, 11, 12, 13, 14, 15, 32,
				  17, 18, 19, 20, 21, 22, 23, 24,
				  25, 26, 27, 28, 29, 30, 31, 48);
}

/*
** f8:
**	extq	z0\.b, z0\.b, z1\.b, #11
**	ret
*/
[[gnu::noipa]] fixed_int8_t
f8 (fixed_int8_t z0, fixed_int8_t z1)
{
  return __builtin_shufflevector (z0, z1,
				  11, 12, 13, 14, 15, 32, 33, 34,
				  35, 36, 37, 38, 39, 40, 41, 42,
				  27, 28, 29, 30, 31, 48, 49, 50,
				  51, 52, 53, 54, 55, 56, 57, 58);
}

/*
** f9:
**	extq	z0\.b, z0\.b, z1\.b, #15
**	ret
*/
[[gnu::noipa]] fixed_int8_t
f9 (fixed_int8_t z0, fixed_int8_t z1)
{
  return __builtin_shufflevector (z0, z1,
				  15, 32, 33, 34, 35, 36, 37, 38,
				  39, 40, 41, 42, 43, 44, 45, 46,
				  31, 48, 49, 50, 51, 52, 53, 54,
				  55, 56, 57, 58, 59, 60, 61, 62);
}
