/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -msve-vector-bits=256 --save-temps" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <stdint.h>

typedef int8_t vnx16qi __attribute__((vector_size (32)));

/* Predicate vector: 1 0 1 0 ... */

#define MASK_32		{ 0, 33, 2, 35, 4, 37, 6, 39, 8, 41,			\
			  10, 43, 12, 45, 14, 47, 16, 49, 18, 51, 		\
			  20, 53, 22, 55, 24, 57, 26, 59, 28, 61, 30, 63 }

#define INDEX_32 vnx16qi

/*
** permute:
**	ptrue	(p[0-7])\.h, vl16
**	sel	z0\.b, \1, z0\.b, z1\.b
**	ret
*/
__SVInt8_t
permute (__SVInt8_t x, __SVInt8_t y)
{
  return __builtin_shuffle ((vnx16qi) x, (vnx16qi) y, (vnx16qi) MASK_32);
}
