/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O -msve-vector-bits=256 --save-temps" } */

#include <stdint.h>

typedef int8_t vnx16qi __attribute__((vector_size (32)));

#define MASK_2(X, Y) (X) ^ (Y), (X + 1) ^ (Y)
#define MASK_4(X, Y) MASK_2 (X, Y), MASK_2 (X + 2, Y)
#define MASK_8(X, Y) MASK_4 (X, Y), MASK_4 (X + 4, Y)
#define MASK_16(X, Y) MASK_8 (X, Y), MASK_8 (X + 8, Y)
#define MASK_32(X, Y) MASK_16 (X, Y), MASK_16 (X + 16, Y)

#define INDEX_32 vnx16qi

#define PERMUTE(TYPE, NUNITS, REV_NUNITS)				\
  TYPE permute_##TYPE##_##REV_NUNITS (TYPE values1, TYPE values2)	\
  {									\
    return __builtin_shuffle						\
      (values1, values2,						\
       ((INDEX_##NUNITS) { MASK_##NUNITS (0, REV_NUNITS - 1) }));	\
  }

#define TEST_ALL(T)				\
  T (vnx16qi, 32, 2)				\
  T (vnx16qi, 32, 4)				\
  T (vnx16qi, 32, 8)

TEST_ALL (PERMUTE)

/* { dg-final { scan-assembler-not {\ttbl\t} } } */

/* { dg-final { scan-assembler-times {\trevb\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d} 1 } } */
/* { dg-final { scan-assembler-times {\trevb\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s} 1 } } */
/* { dg-final { scan-assembler-times {\trevb\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h} 1 } } */
