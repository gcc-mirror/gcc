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

#define VEC_PERM(TYPE, MASKTYPE)			\
TYPE vec_perm_##TYPE (TYPE values, MASKTYPE mask)	\
{							\
  return __builtin_shuffle (values, mask);		\
}

VEC_PERM (vnx2di, vnx2di)
VEC_PERM (vnx4si, vnx4si)
VEC_PERM (vnx8hi, vnx8hi)
VEC_PERM (vnx16qi, vnx16qi)
VEC_PERM (vnx2df, vnx2di)
VEC_PERM (vnx4sf, vnx4si)
VEC_PERM (vnx8hf, vnx8hi)

/* { dg-final { scan-assembler-times {\ttbl\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 2 } } */
/* { dg-final { scan-assembler-times {\ttbl\tz[0-9]+\.s, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\ttbl\tz[0-9]+\.h, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\ttbl\tz[0-9]+\.b, z[0-9]+\.b, z[0-9]+\.b\n} 1 } } */
