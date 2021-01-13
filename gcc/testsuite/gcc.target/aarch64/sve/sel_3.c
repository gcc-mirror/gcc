/* { dg-do assemble { target aarch64_variant_pcs } } */
/* { dg-options "-O2 -msve-vector-bits=256 --save-temps" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <stdint.h>

typedef int8_t vnx16qi __attribute__((vector_size (32)));
typedef int16_t vnx8hi __attribute__((vector_size (32)));
typedef int32_t vnx4si __attribute__((vector_size (32)));
typedef _Float16 vnx8hf __attribute__((vector_size (32)));
typedef float vnx4sf __attribute__((vector_size (32)));

/* Predicate vector: 1 0 0 0 0 0 0 0 ... */

#define MASK_32		{ 0, 33, 34, 35, 36, 37, 38, 39,  \
			  8, 41, 42, 43, 44, 45, 46, 47,  \
			  16, 49, 50, 51, 52, 53, 54, 55, \
			  24, 57, 58, 59, 60, 61, 62, 63  }

/* Predicate vector: 1 0 0 0 ... */

#define MASK_16		{ 0, 17, 18, 19, 4, 21, 22, 23, \
			  8, 25, 26, 27, 12, 29, 30, 31 } 

/* Predicate vector: 1 0 ... */

#define MASK_8		{ 0, 9, 2, 11, 4, 13, 6, 15 }

/*
** permute_vnx16qi:
**	ptrue	(p[0-7])\.d, vl4
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
**	ptrue	(p[0-7])\.d, vl4
**	sel	z0\.h, \1, z0\.h, z1\.h
**	ret
*/
__SVInt16_t
permute_vnx8hi (__SVInt16_t x, __SVInt16_t y)
{
  return __builtin_shuffle ((vnx8hi) x, (vnx8hi) y, (vnx8hi) MASK_16);
}

/*
** permute_vnx4si:
**	ptrue	(p[0-7])\.d, vl4
**	sel	z0\.s, \1, z0\.s, z1\.s
**	ret
*/
__SVInt32_t
permute_vnx4si (__SVInt32_t x, __SVInt32_t y)
{
  return __builtin_shuffle ((vnx4si) x, (vnx4si) y, (vnx4si) MASK_8);
}

/*
** permute_vnx8hf:
**	ptrue	(p[0-7])\.d, vl4
**	sel	z0\.h, \1, z0\.h, z1\.h
**	ret
*/
__SVFloat16_t
permute_vnx8hf (__SVFloat16_t x, __SVFloat16_t y)
{
  return (__SVFloat16_t) __builtin_shuffle ((vnx8hf) x, (vnx8hf) y,
					    (vnx8hi) MASK_16);
}

/*
** permute_vnx4sf:
**	ptrue	(p[0-7])\.d, vl4
**	sel	z0\.s, \1, z0\.s, z1\.s
**	ret
*/
__SVFloat32_t
permute_vnx4sf (__SVFloat32_t x, __SVFloat32_t y)
{
  return __builtin_shuffle ((vnx4sf) x, (vnx4sf) y, (vnx4si) MASK_8);
}
