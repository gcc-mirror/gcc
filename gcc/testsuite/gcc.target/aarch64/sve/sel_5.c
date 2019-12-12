/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -msve-vector-bits=256 --save-temps" } */

#include <stdint.h>

typedef int8_t vnx16qi __attribute__((vector_size (32)));
typedef int16_t vnx8hi __attribute__((vector_size (32)));
typedef int32_t vnx4si __attribute__((vector_size (32)));
typedef int64_t vnx2di __attribute__((vector_size (32)));

typedef _Float16 vnx8hf __attribute__((vector_size (32)));
typedef float vnx4sf __attribute__((vector_size (32)));
typedef double vnx2df __attribute__((vector_size (32)));

/* Predicate vector: 1 0 0 1 ... */

#define MASK_32		{ 0, 33, 34, 3, 4, 37, 38, 7, 8, 41, 42, 11, 12, 45, 46,	\
			  15, 16, 49, 50, 19, 20, 53, 54, 23, 24, 57, 58, 27, 28,	\
			  61, 62, 31 } 

#define MASK_16		{0, 17, 18, 3, 4, 21, 22, 7, 8, 25, 26, 11, 12, 29, 30, 15}
#define MASK_8		{0, 9, 10, 3, 4, 13, 14, 7} 
#define MASK_4		{0, 5, 6, 3}

#define INDEX_32 vnx16qi
#define INDEX_16 vnx8hi
#define INDEX_8 vnx4si
#define INDEX_4 vnx2di

#define PERMUTE(type, nunits)						\
type permute_##type (type x, type y)					\
{									\
  return __builtin_shuffle (x, y, (INDEX_##nunits) MASK_##nunits);	\
}

PERMUTE(vnx16qi, 32)
PERMUTE(vnx8hi, 16)
PERMUTE(vnx4si, 8)
PERMUTE(vnx2di, 4)

PERMUTE(vnx8hf, 16)
PERMUTE(vnx4sf, 8)
PERMUTE(vnx2df, 4)

/* { dg-final { scan-assembler-not {\ttbl\t} } } */

/* { dg-final { scan-assembler-times {\tsel\tz[0-9]+\.b, p[0-9]+, z[0-9]+\.b, z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsel\tz[0-9]+\.h, p[0-9]+, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tsel\tz[0-9]+\.s, p[0-9]+, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tsel\tz[0-9]+\.d, p[0-9]+, z[0-9]+\.d, z[0-9]+\.d\n} 2 } } */
