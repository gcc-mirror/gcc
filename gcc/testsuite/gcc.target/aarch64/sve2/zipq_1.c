/* { dg-options "-O2 -msve-vector-bits=256" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_sve.h>

#pragma GCC target "+sve2p1"

typedef svuint8_t fixed_uint8_t __attribute__((arm_sve_vector_bits(256)));
typedef svbfloat16_t fixed_bfloat16_t __attribute__((arm_sve_vector_bits(256)));
typedef svfloat32_t fixed_float32_t __attribute__((arm_sve_vector_bits(256)));
typedef svint64_t fixed_int64_t __attribute__((arm_sve_vector_bits(256)));

/*
** f1:
**	trn1	z0\.d, z0\.d, z1\.d
**	ret
*/
[[gnu::noipa]] fixed_int64_t
f1 (fixed_int64_t z0, fixed_int64_t z1)
{
  return __builtin_shufflevector (z0, z1, 0, 4, 2, 6);
}

/*
** f2:
**	trn2	z0\.d, z0\.d, z1\.d
**	ret
*/
[[gnu::noipa]] fixed_int64_t
f2 (fixed_int64_t z0, fixed_int64_t z1)
{
  return __builtin_shufflevector (z0, z1, 1, 5, 3, 7);
}

/*
** f3:
**	zipq1	z0\.s, z0\.s, z1\.s
**	ret
*/
[[gnu::noipa]] fixed_float32_t
f3 (fixed_float32_t z0, fixed_float32_t z1)
{
  return __builtin_shufflevector (z0, z1, 0, 8, 1, 9, 4, 12, 5, 13);
}

/*
** f4:
**	zipq2	z0\.s, z0\.s, z1\.s
**	ret
*/
[[gnu::noipa]] fixed_float32_t
f4 (fixed_float32_t z0, fixed_float32_t z1)
{
  return __builtin_shufflevector (z0, z1, 2, 10, 3, 11, 6, 14, 7, 15);
}

/*
** f5:
**	zipq1	z0\.h, z0\.h, z1\.h
**	ret
*/
[[gnu::noipa]] fixed_bfloat16_t
f5 (fixed_bfloat16_t z0, fixed_bfloat16_t z1)
{
  return __builtin_shufflevector (z0, z1,
				  0, 16, 1, 17, 2, 18, 3, 19,
				  8, 24, 9, 25, 10, 26, 11, 27);
}

/*
** f6:
**	zipq2	z0\.h, z0\.h, z1\.h
**	ret
*/
[[gnu::noipa]] fixed_bfloat16_t
f6 (fixed_bfloat16_t z0, fixed_bfloat16_t z1)
{
  return __builtin_shufflevector (z0, z1,
				  4, 20, 5, 21, 6, 22, 7, 23,
				  12, 28, 13, 29, 14, 30, 15, 31);
}

/*
** f7:
**	zipq1	z0\.b, z0\.b, z1\.b
**	ret
*/
[[gnu::noipa]] fixed_uint8_t
f7 (fixed_uint8_t z0, fixed_uint8_t z1)
{
  return __builtin_shufflevector (z0, z1,
				  0, 32, 1, 33, 2, 34, 3, 35,
				  4, 36, 5, 37, 6, 38, 7, 39,
				  16, 48, 17, 49, 18, 50, 19, 51,
				  20, 52, 21, 53, 22, 54, 23, 55);
}

/*
** f8:
**	zipq2	z0\.b, z0\.b, z1\.b
**	ret
*/
[[gnu::noipa]] fixed_uint8_t
f8 (fixed_uint8_t z0, fixed_uint8_t z1)
{
  return __builtin_shufflevector (z0, z1,
				  8, 40, 9, 41, 10, 42, 11, 43,
				  12, 44, 13, 45, 14, 46, 15, 47,
				  24, 56, 25, 57, 26, 58, 27, 59,
				  28, 60, 29, 61, 30, 62, 31, 63);
}
