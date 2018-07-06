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

#define UZP2(TYPE, MASK)				\
TYPE uzp2_##TYPE (TYPE values1, TYPE values2)		\
{							\
  return __builtin_shuffle (values1, values2, MASK);	\
}

UZP2 (vnx2di,  ((vnx2di) { 1, 3, 5, 7 }));
UZP2 (vnx4si,  ((vnx4si) { 1, 3, 5, 7, 9, 11, 13, 15 }));
UZP2 (vnx8hi,  ((vnx8hi) { 1, 3, 5, 7, 9, 11, 13, 15,
			   17, 19, 21, 23, 25, 27, 29, 31 }));
UZP2 (vnx16qi, ((vnx16qi) { 1, 3, 5, 7, 9, 11, 13, 15,
			    17, 19, 21, 23, 25, 27, 29, 31,
			    33, 35, 37, 39, 41, 43, 45, 47,
			    49, 51, 53, 55, 57, 59, 61, 63 }));
UZP2 (vnx2df,  ((vnx2di) { 1, 3, 5, 7 }));
UZP2 (vnx4sf,  ((vnx4si) { 1, 3, 5, 7, 9, 11, 13, 15 }));
UZP2 (vnx8hf,  ((vnx8hi) { 1, 3, 5, 7, 9, 11, 13, 15,
			   17, 19, 21, 23, 25, 27, 29, 31 }));

/* { dg-final { scan-assembler-not {\ttbl\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} } } */
/* { dg-final { scan-assembler-not {\ttbl\tz[0-9]+\.s, z[0-9]+\.s, z[0-9]+\.s\n} } } */
/* { dg-final { scan-assembler-not {\ttbl\tz[0-9]+\.h, z[0-9]+\.h, z[0-9]+\.h\n} } } */
/* { dg-final { scan-assembler-not {\ttbl\tz[0-9]+\.b, z[0-9]+\.b, z[0-9]+\.b\n} } } */

/* { dg-final { scan-assembler-times {\tuzp2\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 2 } } */
/* { dg-final { scan-assembler-times {\tuzp2\tz[0-9]+\.s, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tuzp2\tz[0-9]+\.h, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tuzp2\tz[0-9]+\.b, z[0-9]+\.b, z[0-9]+\.b\n} 1 } } */
