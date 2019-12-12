/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -msve-vector-bits=256 --save-temps" } */

#include <stdint.h>

typedef int8_t vnx16qi __attribute__((vector_size (32)));
typedef int16_t vnx8hi __attribute__((vector_size (32)));
typedef int32_t vnx4si __attribute__((vector_size (32)));

typedef _Float16 vnx8hf __attribute__((vector_size (32)));
typedef float vnx4sf __attribute__((vector_size (32)));

/* Predicate vector: 1 0 0 0 ... */

#define MASK_32		{ 0, 33, 34, 35, 4, 37, 38, 39, 8, 41, 42, 43, 12,		\
			  45, 46, 47, 16, 49, 50, 51, 20, 53, 54, 55, 24,		\
			  57, 58, 59, 28, 61, 62, 63 } 

/* Predicate vector: 1 0 1 0 ... */

#define MASK_16		{0, 17, 2, 19, 4, 21, 6, 23, 8, 25, 10, 27, 12, 29, 14, 31}

#define INDEX_32 vnx16qi
#define INDEX_16 vnx8hi

#define PERMUTE(type, nunits)						\
type permute_##type (type x, type y)					\
{									\
  return __builtin_shuffle (x, y, (INDEX_##nunits) MASK_##nunits);	\
}

PERMUTE(vnx16qi, 32)
PERMUTE(vnx8hi, 16)
PERMUTE(vnx8hf, 16)

/* { dg-final { scan-assembler-not {\ttbl\t} } } */

/* { dg-final { scan-assembler-times {\tsel\tz[0-9]+\.b, p[0-9]+, z[0-9]+\.b, z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsel\tz[0-9]+\.h, p[0-9]+, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */

/* { dg-final { scan-assembler-times {\tptrue\tp[0-9]+\.s, vl8\n} 3 } } */
