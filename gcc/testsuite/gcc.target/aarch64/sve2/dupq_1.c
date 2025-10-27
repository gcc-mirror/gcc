/* { dg-options "-O2 -msve-vector-bits=256" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_sve.h>

#pragma GCC target "+sve2p1"

typedef svint8_t fixed_uint8_t __attribute__((arm_sve_vector_bits(256)));
typedef svuint16_t fixed_uint16_t __attribute__((arm_sve_vector_bits(256)));
typedef svint32_t fixed_int32_t __attribute__((arm_sve_vector_bits(256)));
typedef svuint64_t fixed_uint64_t __attribute__((arm_sve_vector_bits(256)));

/*
** f1:
**	trn1	z0\.d, z0\.d, z0\.d
**	ret
*/
[[gnu::noipa]] fixed_uint64_t
f1 (fixed_uint64_t z0)
{
  return __builtin_shufflevector (z0, z0, 0, 0, 2, 2);
}

/*
** f2:
**	trn2	z0\.d, z0\.d, z0\.d
**	ret
*/
[[gnu::noipa]] fixed_uint64_t
f2 (fixed_uint64_t z0)
{
  return __builtin_shufflevector (z0, z0, 1, 1, 3, 3);
}

/*
** f3:
**	dupq	z0\.s, z0\.s\[0\]
**	ret
*/
[[gnu::noipa]] fixed_int32_t
f3 (fixed_int32_t z0)
{
  return __builtin_shufflevector (z0, z0, 0, 0, 0, 0, 4, 4, 4, 4);
}

/*
** f4:
**	dupq	z0\.s, z0\.s\[1\]
**	ret
*/
[[gnu::noipa]] fixed_int32_t
f4 (fixed_int32_t z0)
{
  return __builtin_shufflevector (z0, z0, 1, 1, 1, 1, 5, 5, 5, 5);
}

/*
** f5:
**	dupq	z0\.s, z0\.s\[2\]
**	ret
*/
[[gnu::noipa]] fixed_int32_t
f5 (fixed_int32_t z0)
{
  return __builtin_shufflevector (z0, z0, 2, 2, 2, 2, 6, 6, 6, 6);
}

/*
** f6:
**	dupq	z0\.s, z0\.s\[3\]
**	ret
*/
[[gnu::noipa]] fixed_int32_t
f6 (fixed_int32_t z0)
{
  return __builtin_shufflevector (z0, z0, 3, 3, 3, 3, 7, 7, 7, 7);
}

/*
** f7:
**	dupq	z0\.h, z0\.h\[0\]
**	ret
*/
[[gnu::noipa]] fixed_uint16_t
f7 (fixed_uint16_t z0)
{
  return __builtin_shufflevector (z0, z0,
				  0, 0, 0, 0, 0, 0, 0, 0,
				  8, 8, 8, 8, 8, 8, 8, 8);
}


/*
** f8:
**	dupq	z0\.h, z0\.h\[5\]
**	ret
*/
[[gnu::noipa]] fixed_uint16_t
f8 (fixed_uint16_t z0)
{
  return __builtin_shufflevector (z0, z0,
				  5, 5, 5, 5, 5, 5, 5, 5,
				  13, 13, 13, 13, 13, 13, 13, 13);
}

/*
** f9:
**	dupq	z0\.h, z0\.h\[7\]
**	ret
*/
[[gnu::noipa]] fixed_uint16_t
f9 (fixed_uint16_t z0)
{
  return __builtin_shufflevector (z0, z0,
				  7, 7, 7, 7, 7, 7, 7, 7,
				  15, 15, 15, 15, 15, 15, 15, 15);
}

/*
** f10:
**	dupq	z0\.b, z0\.b\[0\]
**	ret
*/
[[gnu::noipa]] fixed_uint8_t
f10 (fixed_uint8_t z0)
{
  return __builtin_shufflevector (z0, z0,
				  0, 0, 0, 0, 0, 0, 0, 0,
				  0, 0, 0, 0, 0, 0, 0, 0,
				  16, 16, 16, 16, 16, 16, 16, 16,
				  16, 16, 16, 16, 16, 16, 16, 16);
}

/*
** f11:
**	dupq	z0\.b, z0\.b\[13\]
**	ret
*/
[[gnu::noipa]] fixed_uint8_t
f11 (fixed_uint8_t z0)
{
  return __builtin_shufflevector (z0, z0,
				  13, 13, 13, 13, 13, 13, 13, 13,
				  13, 13, 13, 13, 13, 13, 13, 13,
				  29, 29, 29, 29, 29, 29, 29, 29,
				  29, 29, 29, 29, 29, 29, 29, 29);
}

/*
** f12:
**	dupq	z0\.b, z0\.b\[15\]
**	ret
*/
[[gnu::noipa]] fixed_uint8_t
f12 (fixed_uint8_t z0)
{
  return __builtin_shufflevector (z0, z0,
				  15, 15, 15, 15, 15, 15, 15, 15,
				  15, 15, 15, 15, 15, 15, 15, 15,
				  31, 31, 31, 31, 31, 31, 31, 31,
				  31, 31, 31, 31, 31, 31, 31, 31);
}
