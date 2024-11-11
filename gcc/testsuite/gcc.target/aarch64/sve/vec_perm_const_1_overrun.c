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

#define VEC_PERM_CONST_OVERRUN(TYPE, MASK)			\
TYPE vec_perm_overrun_##TYPE (TYPE values1, TYPE values2)	\
{								\
  return __builtin_shuffle (values1, values2, MASK);		\
}

VEC_PERM_CONST_OVERRUN (vnx2di,  ((vnx2di)  { 4 + (8 * 1), 3 + (8 * 1),
					      6 + (8 * 2), 1 + (8 * 3) }));
VEC_PERM_CONST_OVERRUN (vnx4si,  ((vnx4si)  { 3 + (16 * 3), 9 + (16 * 4),
					      11 + (16 * 5), 12 + (16 * 3),
					      2 + (16 * 2), 4 + (16 * 1),
					      4 + (16 * 2), 2 + (16 * 1) }));
VEC_PERM_CONST_OVERRUN (vnx8hi,  ((vnx8hi)  { 8 + (32 * 3), 27 + (32 * 1),
					      5 + (32 * 3), 4 + (32 * 3),
					      21 + (32 * 1), 12 + (32 * 3),
					      13 + (32 * 3), 0 + (32 * 1),
					      22 + (32 * 2), 1 + (32 * 2),
					      8 + (32 * 2), 9 + (32 * 1),
					      3 + (32 * 2), 24 + (32 * 2),
					      15 + (32 * 1), 1 + (32 * 1) }));
VEC_PERM_CONST_OVERRUN (vnx16qi, ((vnx16qi) { 13 + (64 * 2), 31 + (64 * 2),
					      11 + (64 * 2), 2 + (64 * 1),
					      48 + (64 * 1), 28 + (64 * 2),
					      3 + (64 * 2), 4 + (64 * 3),
					      54 + (64 * 1), 11 + (64 * 2),
					      30 + (64 * 2), 1 + (64 * 1),
					      0 + (64 * 1), 61 + (64 * 2),
					      2 + (64 * 3), 3 + (64 * 2),
					      4 + (64 * 3), 5 + (64 * 3),
					      11 + (64 * 3), 63 + (64 * 1),
					      24 + (64 * 1), 11 + (64 * 3),
					      42 + (64 * 3), 39 + (64 * 2),
					      2 + (64 * 2), 57 + (64 * 3),
					      22 + (64 * 3), 11 + (64 * 2),
					      6 + (64 * 2), 16 + (64 * 2),
					      18 + (64 * 2), 21 + (64 * 3) }));
VEC_PERM_CONST_OVERRUN (vnx2df,  ((vnx2di)  { 7 + (8 * 1), 3 + (8 * 3),
					      2 + (8 * 5), 1 + (8 * 3) }));
VEC_PERM_CONST_OVERRUN (vnx4sf,  ((vnx4si)  { 1 + (16 * 1), 9 + (16 * 2),
					      13 + (16 * 2), 11 + (16 * 3),
					      2 + (16 * 2), 5 + (16 * 2),
					      4 + (16 * 4), 2 + (16 * 3) }));
VEC_PERM_CONST_OVERRUN (vnx8hf,  ((vnx8hi)  { 8 + (32 * 3), 27 + (32 * 1),
					      5 + (32 * 3), 4 + (32 * 3),
					      21 + (32 * 1), 12 + (32 * 3),
					      13 + (32 * 3), 0 + (32 * 1),
					      22 + (32 * 2), 1 + (32 * 2),
					      8 + (32 * 2), 9 + (32 * 1),
					      3 + (32 * 2), 24 + (32 * 2),
					      15 + (32 * 1), 1 + (32 * 1) }));

/* { dg-final { scan-assembler-times {\ttbl\tz[0-9]+\.d, {z[0-9]+\.d}, z[0-9]+\.d\n} 4 } } */
/* { dg-final { scan-assembler-times {\ttbl\tz[0-9]+\.s, {z[0-9]+\.s}, z[0-9]+\.s\n} 4 } } */
/* { dg-final { scan-assembler-times {\ttbl\tz[0-9]+\.h, {z[0-9]+\.h}, z[0-9]+\.h\n} 4 } } */
/* { dg-final { scan-assembler-times {\ttbl\tz[0-9]+\.b, {z[0-9]+\.b}, z[0-9]+\.b\n} 2 } } */
