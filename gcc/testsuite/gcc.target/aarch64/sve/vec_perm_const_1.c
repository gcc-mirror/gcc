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

#define VEC_PERM_CONST(TYPE, MASK)			\
TYPE __attribute__ ((noinline, noclone)) 		\
vec_perm_##TYPE (TYPE values1, TYPE values2)		\
{							\
  return __builtin_shuffle (values1, values2, MASK);	\
}

VEC_PERM_CONST (vnx2di,  ((vnx2di)  { 4, 3, 6, 1 }));
VEC_PERM_CONST (vnx4si,  ((vnx4si)  { 3, 9, 11, 12, 2, 4, 4, 2 }));
VEC_PERM_CONST (vnx8hi,  ((vnx8hi)  { 8, 27, 5, 4, 21, 12, 13, 0,
				      22, 1, 8, 9, 3, 24, 15, 1 }));
VEC_PERM_CONST (vnx16qi, ((vnx16qi) { 13, 31, 11, 2, 48, 28, 3, 4,
				      54, 11, 30, 1, 0, 61, 2, 3,
				      4, 5, 11, 63, 24, 11, 42, 39,
				      2, 57, 22, 11, 6, 16, 18, 21 }));
VEC_PERM_CONST (vnx2df,  ((vnx2di) { 7, 3, 2, 1 }));
VEC_PERM_CONST (vnx4sf,  ((vnx4si) { 1, 9, 13, 11, 2, 5, 4, 2 }));
VEC_PERM_CONST (vnx8hf,  ((vnx8hi) { 8, 27, 5, 4, 21, 12, 13, 0,
				     22, 1, 8, 9, 3, 24, 15, 1 }));

/* { dg-final { scan-assembler-times {\ttbl\tz[0-9]+\.d, {z[0-9]+\.d}, z[0-9]+\.d\n} 4 } } */
/* { dg-final { scan-assembler-times {\ttbl\tz[0-9]+\.s, {z[0-9]+\.s}, z[0-9]+\.s\n} 4 } } */
/* { dg-final { scan-assembler-times {\ttbl\tz[0-9]+\.h, {z[0-9]+\.h}, z[0-9]+\.h\n} 4 } } */
/* { dg-final { scan-assembler-times {\ttbl\tz[0-9]+\.b, {z[0-9]+\.b}, z[0-9]+\.b\n} 2 } } */
