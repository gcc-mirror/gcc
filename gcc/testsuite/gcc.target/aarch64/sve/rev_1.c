/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O -msve-vector-bits=256 --save-temps" } */

#include <stdint.h>

typedef int8_t vnx16qi __attribute__((vector_size (32)));
typedef int16_t vnx8hi __attribute__((vector_size (32)));
typedef int32_t vnx4si __attribute__((vector_size (32)));
typedef int64_t vnx2di __attribute__((vector_size (32)));
typedef _Float16 vnx8hf __attribute__((vector_size (32)));
typedef float vnx4sf __attribute__((vector_size (32)));
typedef double vnx2df __attribute__((vector_size (32)));

#define MASK_2(X, Y) (Y) - 1 - (X), (Y) - 2 - (X)
#define MASK_4(X, Y) MASK_2 (X, Y), MASK_2 (X + 2, Y)
#define MASK_8(X, Y) MASK_4 (X, Y), MASK_4 (X + 4, Y)
#define MASK_16(X, Y) MASK_8 (X, Y), MASK_8 (X + 8, Y)
#define MASK_32(X, Y) MASK_16 (X, Y), MASK_16 (X + 16, Y)

#define INDEX_32 vnx16qi
#define INDEX_16 vnx8hi
#define INDEX_8 vnx4si
#define INDEX_4 vnx2di

#define PERMUTE(TYPE, NUNITS)						\
  TYPE permute_##TYPE (TYPE values1, TYPE values2)			\
  {									\
    return __builtin_shuffle						\
      (values1, values2,						\
       ((INDEX_##NUNITS) { MASK_##NUNITS (0, NUNITS) }));		\
  }

#define TEST_ALL(T)				\
  T (vnx16qi, 32)				\
  T (vnx8hi, 16)				\
  T (vnx4si, 8)					\
  T (vnx2di, 4)					\
  T (vnx8hf, 16)				\
  T (vnx4sf, 8)					\
  T (vnx2df, 4)

TEST_ALL (PERMUTE)

/* { dg-final { scan-assembler-not {\ttbl\t} } } */

/* { dg-final { scan-assembler-times {\trev\tz[0-9]+\.b, z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\trev\tz[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\trev\tz[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\trev\tz[0-9]+\.d, z[0-9]+\.d\n} 2 } } */
