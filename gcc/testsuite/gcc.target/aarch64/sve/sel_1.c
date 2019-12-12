/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -msve-vector-bits=256 --save-temps" } */

#include <stdint.h>

typedef int8_t vnx16qi __attribute__((vector_size (32)));

/* Predicate vector: 1 0 1 0 ... */

#define MASK_32		{ 0, 33, 2, 35, 4, 37, 6, 39, 8, 41,			\
			  10, 43, 12, 45, 14, 47, 16, 49, 18, 51, 		\
			  20, 53, 22, 55, 24, 57, 26, 59, 28, 61, 30, 63 }

#define INDEX_32 vnx16qi

#define PERMUTE(type, nunits)						\
type permute_##type (type x, type y)					\
{									\
  return __builtin_shuffle (x, y, (INDEX_##nunits) MASK_##nunits);	\
}

PERMUTE(vnx16qi, 32)

/* { dg-final { scan-assembler-not {\ttbl\t} } } */

/* { dg-final { scan-assembler-times {\tsel\tz[0-9]+\.b, p[0-9]+, z[0-9]+\.b, z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-9]+\.h, vl16\n} 1 } } */
