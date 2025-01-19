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

#define VEC_PERM_SINGLE(TYPE, MASK)			\
TYPE vec_perm_##TYPE (TYPE values1, TYPE values2)	\
{							\
  return __builtin_shuffle (values1, values2, MASK);	\
}

VEC_PERM_SINGLE (vnx2di,  ((vnx2di)  { 0, 3, 2, 1 }));
VEC_PERM_SINGLE (vnx4si,  ((vnx4si)  { 3, 7, 1, 0, 2, 4, 4, 2 }));
VEC_PERM_SINGLE (vnx8hi,  ((vnx8hi)  { 8, 7, 5, 4, 11, 12, 13, 0,
				       1, 1, 8, 9, 3, 14, 15, 1 }));
VEC_PERM_SINGLE (vnx16qi, ((vnx16qi) { 13, 21, 11, 2, 8, 28, 3, 4,
				       14, 11, 30, 1, 0, 31, 2, 3,
				       4, 5, 11, 23, 24, 11, 12, 9,
				       2, 7, 22, 11, 6, 16, 18, 21 }));
VEC_PERM_SINGLE (vnx2df,  ((vnx2di)  { 3, 3, 1, 1 }));
VEC_PERM_SINGLE (vnx4sf,  ((vnx4si)  { 4, 5, 6, 0, 2, 7, 4, 2 }));
VEC_PERM_SINGLE (vnx8hf,  ((vnx8hi)  { 8, 7, 5, 4, 11, 12, 13, 0,
				       1, 1, 8, 9, 3, 14, 15, 1 }));

/* { dg-final { scan-assembler-times {\ttbl\tz[0-9]+\.d, {z[0-9]+\.d}, z[0-9]+\.d\n} 2 } } */
/* { dg-final { scan-assembler-times {\ttbl\tz[0-9]+\.s, {z[0-9]+\.s}, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\ttbl\tz[0-9]+\.h, {z[0-9]+\.h}, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\ttbl\tz[0-9]+\.b, {z[0-9]+\.b}, z[0-9]+\.b\n} 1 } } */
