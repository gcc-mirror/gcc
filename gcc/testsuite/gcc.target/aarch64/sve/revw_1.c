/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O -msve-vector-bits=256 --save-temps" } */

#include <stdint.h>

typedef uint32_t vnx4si __attribute__((vector_size (32)));
typedef float vnx4sf __attribute__((vector_size (32)));

#define MASK_2(X, Y) (X) ^ (Y), (X + 1) ^ (Y)
#define MASK_4(X, Y) MASK_2 (X, Y), MASK_2 (X + 2, Y)
#define MASK_8(X, Y) MASK_4 (X, Y), MASK_4 (X + 4, Y)

#define INDEX_8 vnx4si

#define PERMUTE(TYPE, NUNITS, REV_NUNITS)				\
  TYPE permute_##TYPE##_##REV_NUNITS (TYPE values1, TYPE values2)	\
  {									\
    return __builtin_shuffle						\
      (values1, values2,						\
       ((INDEX_##NUNITS) { MASK_##NUNITS (0, REV_NUNITS - 1) }));	\
  }

#define TEST_ALL(T)				\
  T (vnx4si, 8, 2)				\
  T (vnx4sf, 8, 2)

TEST_ALL (PERMUTE)

/* { dg-final { scan-assembler-not {\ttbl\t} } } */

/* { dg-final { scan-assembler-times {\trevw\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d} 2 } } */
