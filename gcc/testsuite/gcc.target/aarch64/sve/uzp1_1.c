/* { dg-do compile } */
/* { dg-options "-O -msve-vector-bits=256" } */

#include <stdint.h>

typedef int64_t vnx2di __attribute__((vector_size (32)));
typedef int32_t vnx4si __attribute__((vector_size (32)));
typedef int16_t vnx8hi __attribute__((vector_size (32)));
typedef int8_t vnx16qi __attribute__((vector_size (32)));
typedef double vnx2df __attribute__((vector_size (32)));
typedef float vnx4sf __attribute__((vector_size (32)));
typedef _Float16 vnx8hf __attribute__((vector_size (32)));

#define UZP1(TYPE, MASK)				\
TYPE uzp1_##TYPE (TYPE values1, TYPE values2)		\
{							\
  return __builtin_shuffle (values1, values2, MASK);	\
}


UZP1 (vnx2di,  ((vnx2di) { 0, 2, 4, 6 }));
UZP1 (vnx4si,  ((vnx4si) { 0, 2, 4, 6, 8, 10, 12, 14 }));
UZP1 (vnx8hi,  ((vnx8hi) { 0, 2, 4, 6, 8, 10, 12, 14,
			   16, 18, 20, 22, 24, 26, 28, 30 }));
UZP1 (vnx16qi, ((vnx16qi) { 0, 2, 4, 6, 8, 10, 12, 14,
			    16, 18, 20, 22, 24, 26, 28, 30,
			    32, 34, 36, 38, 40, 42, 44, 46,
			    48, 50, 52, 54, 56, 58, 60, 62 }));
UZP1 (vnx2df,  ((vnx2di) { 0, 2, 4, 6 }));
UZP1 (vnx4sf,  ((vnx4si) { 0, 2, 4, 6, 8, 10, 12, 14 }));
UZP1 (vnx8hf,  ((vnx8hi) { 0, 2, 4, 6, 8, 10, 12, 14,
			   16, 18, 20, 22, 24, 26, 28, 30 }));

/* { dg-final { scan-assembler-not {\ttbl\t} } } */
/* { dg-final { scan-assembler-not {\ttbl\t} } } */
/* { dg-final { scan-assembler-not {\ttbl\t} } } */
/* { dg-final { scan-assembler-not {\ttbl\t} } } */

/* { dg-final { scan-assembler-times {\tuzp1\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 2 } } */
/* { dg-final { scan-assembler-times {\tuzp1\tz[0-9]+\.s, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tuzp1\tz[0-9]+\.h, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tuzp1\tz[0-9]+\.b, z[0-9]+\.b, z[0-9]+\.b\n} 1 } } */
