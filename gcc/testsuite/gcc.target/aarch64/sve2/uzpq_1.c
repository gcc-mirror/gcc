/* { dg-options "-O2 -msve-vector-bits=256" } */
/* { dg-final { check-function-bodies "**" "" "" { target { le } } } } */

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
fixed_int64_t
f1 (fixed_int64_t z0, fixed_int64_t z1)
{
  return __builtin_shufflevector (z0, z1, 0, 4, 2, 6);
}

/*
** f2:
**	trn2	z0\.d, z0\.d, z1\.d
**	ret
*/
fixed_int64_t
f2 (fixed_int64_t z0, fixed_int64_t z1)
{
  return __builtin_shufflevector (z0, z1, 1, 5, 3, 7);
}

/*
** f3:
**	uzpq1	z0\.s, z0\.s, z1\.s
**	ret
*/
fixed_float32_t
f3 (fixed_float32_t z0, fixed_float32_t z1)
{
  return __builtin_shufflevector (z0, z1, 0, 2, 8, 10, 4, 6, 12, 14);
}

/*
** f4:
**	uzpq2	z0\.s, z0\.s, z1\.s
**	ret
*/
fixed_float32_t
f4 (fixed_float32_t z0, fixed_float32_t z1)
{
  return __builtin_shufflevector (z0, z1, 1, 3, 9, 11, 5, 7, 13, 15);
}

/*
** f5:
**	uzpq1	z0\.h, z0\.h, z1\.h
**	ret
*/
fixed_bfloat16_t
f5 (fixed_bfloat16_t z0, fixed_bfloat16_t z1)
{
  return __builtin_shufflevector (z0, z1,
				  0, 2, 4, 6, 16, 18, 20, 22,
				  8, 10, 12, 14, 24, 26, 28, 30);
}

/*
** f6:
**	uzpq2	z0\.h, z0\.h, z1\.h
**	ret
*/
fixed_bfloat16_t
f6 (fixed_bfloat16_t z0, fixed_bfloat16_t z1)
{
  return __builtin_shufflevector (z0, z1,
				  1, 3, 5, 7, 17, 19, 21, 23,
				  9, 11, 13, 15, 25, 27, 29, 31);
}

/*
** f7:
**	uzpq1	z0\.b, z0\.b, z1\.b
**	ret
*/
fixed_uint8_t
f7 (fixed_uint8_t z0, fixed_uint8_t z1)
{
  return __builtin_shufflevector (z0, z1,
				  0, 2, 4, 6, 8, 10, 12, 14,
				  32, 34, 36, 38, 40, 42, 44, 46,
				  16, 18, 20, 22, 24, 26, 28, 30,
				  48, 50, 52, 54, 56, 58, 60, 62);
}

/*
** f8:
**	uzpq2	z0\.b, z0\.b, z1\.b
**	ret
*/
fixed_uint8_t
f8 (fixed_uint8_t z0, fixed_uint8_t z1)
{
  return __builtin_shufflevector (z0, z1,
				  1, 3, 5, 7, 9, 11, 13, 15,
				  33, 35, 37, 39, 41, 43, 45, 47,
				  17, 19, 21, 23, 25, 27, 29, 31,
				  49, 51, 53, 55, 57, 59, 61, 63);
}
