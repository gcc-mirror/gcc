/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -msve-vector-bits=256 --save-temps" } */

#include <stdint.h>

typedef int32_t vnx4si __attribute__((vector_size (32)));
typedef int64_t vnx2di __attribute__((vector_size (32)));

typedef float vnx4sf __attribute__((vector_size (32)));
typedef double vnx2df __attribute__((vector_size (32)));

/* Predicate vector: 1 0 0 0 ... */

#define MASK_32		{ 0, 33, 34, 35, 4, 37, 38, 39, 8, 41, 42, 43, 12,		\
			  45, 46, 47, 16, 49, 50, 51, 20, 53, 54, 55, 24,		\
			  57, 58, 59, 28, 61, 62, 63 } 

#define MASK_16		{0, 17, 18, 19, 4, 21, 22, 23, 8, 25, 26, 27, 12, 29, 30, 31} 
#define MASK_8		{0, 9, 10, 11, 4, 13, 14, 15}
#define MASK_4		{0, 5, 6, 7}

#define INDEX_8 vnx4si
#define INDEX_4 vnx2di

#define PERMUTE(type, nunits)						\
type permute_##type (type x, type y)					\
{									\
  return __builtin_shuffle (x, y, (INDEX_##nunits) MASK_##nunits);	\
}

PERMUTE(vnx4si, 8)
PERMUTE(vnx2di, 4)

PERMUTE(vnx4sf, 8)
PERMUTE(vnx2df, 4)

/* { dg-final { scan-assembler-not {\ttbl\t} } } */

/* { dg-final { scan-assembler-times {\tsel\tz[0-9]+\.s, p[0-9]+, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tsel\tz[0-9]+\.d, p[0-9]+, z[0-9]+\.d, z[0-9]+\.d\n} 2 } } */

/* { dg-final { scan-assembler-times {\tptrue\tp[0-9]+\.d, vl4\n} 2 } } */
