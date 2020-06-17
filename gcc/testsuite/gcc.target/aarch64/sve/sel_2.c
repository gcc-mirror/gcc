/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -msve-vector-bits=256 --save-temps" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <stdint.h>

typedef int8_t vnx16qi __attribute__((vector_size (32)));
typedef int16_t vnx8hi __attribute__((vector_size (32)));

typedef _Float16 vnx8hf __attribute__((vector_size (32)));

/* Predicate vector: 1 0 0 0 ... */

#define MASK_32		{ 0, 33, 34, 35, 4, 37, 38, 39, 8, 41, 42, 43, 12,		\
			  45, 46, 47, 16, 49, 50, 51, 20, 53, 54, 55, 24,		\
			  57, 58, 59, 28, 61, 62, 63 } 

/* Predicate vector: 1 0 1 0 ... */

#define MASK_16		{0, 17, 2, 19, 4, 21, 6, 23, 8, 25, 10, 27, 12, 29, 14, 31}

/*
** permute_vnx16qi:
**	ptrue	(p[0-7])\.s, vl8
**	sel	z0\.b, \1, z0\.b, z1\.b
**	ret
*/
__SVInt8_t
permute_vnx16qi (__SVInt8_t x, __SVInt8_t y)
{
  return __builtin_shuffle ((vnx16qi) x, (vnx16qi) y, (vnx16qi) MASK_32);
}

/*
** permute_vnx8hi:
**	ptrue	(p[0-7])\.s, vl8
**	sel	z0\.h, \1, z0\.h, z1\.h
**	ret
*/
__SVInt16_t
permute_vnx8hi (__SVInt16_t x, __SVInt16_t y)
{
  return __builtin_shuffle ((vnx8hi) x, (vnx8hi) y, (vnx8hi) MASK_16);
}

/*
** permute_vnx8hf:
**	ptrue	(p[0-7])\.s, vl8
**	sel	z0\.h, \1, z0\.h, z1\.h
**	ret
*/
__SVFloat16_t
permute_vnx8hf (__SVFloat16_t x, __SVFloat16_t y)
{
  return (__SVFloat16_t) __builtin_shuffle ((vnx8hf) x, (vnx8hf) y,
					    (vnx8hi) MASK_16);
}
