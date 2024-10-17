/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d -mrvv-max-lmul=m8" } */

#include "perm.h"

#define MASKE_2(x, y) (x), (x + 2)
#define MASKE_4(x, y) MASKE_2 (x, y), MASKE_2 (x + 4, y)
#define MASKE_8(x, y) MASKE_4 (x, y), MASKE_4 (x + 8, y)
#define MASKE_16(x, y) MASKE_8 (x, y), MASKE_8 (x + 16, y)
#define MASKE_32(x, y) MASKE_16 (x, y), MASKE_16 (x + 32, y)
#define MASKE_64(x, y) MASKE_32 (x, y), MASKE_32 (x + 64, y)

#define MASKO_2(x, y) (x + 1), (x + 3)
#define MASKO_4(x, y) MASKO_2 (x, y), MASKO_2 (x + 4, y)
#define MASKO_8(x, y) MASKO_4 (x, y), MASKO_4 (x + 8, y)
#define MASKO_16(x, y) MASKO_8 (x, y), MASKO_8 (x + 16, y)
#define MASKO_32(x, y) MASKO_16 (x, y), MASKO_16 (x + 32, y)
#define MASKO_64(x, y) MASKO_32 (x, y), MASKO_32 (x + 64, y)

#define PERMUTE1(TYPE, NUNITS)                                                 \
  __attribute__ ((noipa)) void permute1_##TYPE (TYPE values1, TYPE values2,    \
						TYPE *out)                     \
  {                                                                            \
    TYPE v = __builtin_shufflevector (values1, values2,                        \
				      MASKE_##NUNITS (0, NUNITS));             \
    *(TYPE *) out = v;                                                         \
  }

#define PERMUTE2(TYPE, NUNITS)                                                 \
  __attribute__ ((noipa)) void permute2_##TYPE (TYPE values1, TYPE values2,    \
						TYPE *out)                     \
  {                                                                            \
    TYPE v = __builtin_shufflevector (values1, values2,                        \
				      MASKO_##NUNITS (0, NUNITS));             \
    *(TYPE *) out = v;                                                         \
  }

#define TEST_ALL(T)                                                            \
  T (vnx4qi, 4)                                                                \
  T (vnx8qi, 8)                                                                \
  T (vnx16qi, 16)                                                              \
  T (vnx32qi, 32)                                                              \
  T (vnx64qi, 64)                                                              \
  T (vnx4hi, 4)                                                                \
  T (vnx8hi, 8)                                                                \
  T (vnx16hi, 16)                                                              \
  T (vnx32hi, 32)                                                              \
  T (vnx64hi, 64)                                                              \
  T (vnx4si, 4)                                                                \
  T (vnx8si, 8)                                                                \
  T (vnx16si, 16)                                                              \
  T (vnx32si, 32)                                                              \
  T (vnx4di, 4)                                                                \
  T (vnx8di, 8)                                                                \
  T (vnx16di, 16)                                                              \
  T (vnx4sf, 4)                                                                \
  T (vnx8sf, 8)                                                                \
  T (vnx16sf, 16)                                                              \
  T (vnx32sf, 32)                                                              \
  T (vnx4df, 4)                                                                \
  T (vnx8df, 8)                                                                \
  T (vnx16df, 16)

TEST_ALL (PERMUTE1)
TEST_ALL (PERMUTE2)

/* { dg-final { scan-assembler-times "vslideup" 48 } } */
/* { dg-final { scan-assembler-times "vcompress" 96 } } */
