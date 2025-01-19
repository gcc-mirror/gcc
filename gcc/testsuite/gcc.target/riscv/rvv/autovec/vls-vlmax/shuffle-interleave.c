/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d -mrvv-max-lmul=m8" } */

#include "perm.h"

#define MASKL_2(x, y) (x), (x + y)
#define MASKL_4(x, y) MASKL_2 (x, y), MASKL_2 (x + 1, y)
#define MASKL_8(x, y) MASKL_4 (x, y), MASKL_4 (x + 2, y)
#define MASKL_16(x, y) MASKL_8 (x, y), MASKL_8 (x + 4, y)
#define MASKL_32(x, y) MASKL_16 (x, y), MASKL_16 (x + 8, y)
#define MASKL_64(x, y) MASKL_32 (x, y), MASKL_32 (x + 16, y)

#define MASKH_2(x, y) (x + y / 2), (x + y / 2 + y)
#define MASKH_4(x, y) MASKH_2 (x, y), MASKH_2 (x + 1, y)
#define MASKH_8(x, y) MASKH_4 (x, y), MASKH_4 (x + 2, y)
#define MASKH_16(x, y) MASKH_8 (x, y), MASKH_8 (x + 4, y)
#define MASKH_32(x, y) MASKH_16 (x, y), MASKH_16 (x + 8, y)
#define MASKH_64(x, y) MASKH_32 (x, y), MASKH_32 (x + 16, y)

#define PERMUTE1(TYPE, NUNITS)                                                 \
  __attribute__ ((noipa)) void permute1_##TYPE (TYPE values1, TYPE values2,    \
						TYPE *out)                     \
  {                                                                            \
    TYPE v = __builtin_shufflevector (values1, values2,                        \
				      MASKL_##NUNITS (0, NUNITS));             \
    *(TYPE *) out = v;                                                         \
  }

#define PERMUTE2(TYPE, NUNITS)                                                 \
  __attribute__ ((noipa)) void permute2_##TYPE (TYPE values1, TYPE values2,    \
						TYPE *out)                     \
  {                                                                            \
    TYPE v = __builtin_shufflevector (values1, values2,                        \
				      MASKH_##NUNITS (0, NUNITS));             \
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

/* { dg-final { scan-assembler-times "vslideup" 24 } } */
/* { dg-final { scan-assembler-times "vslidedown" 24 } } */
/* { dg-final { scan-assembler-times "vrgather" 48 } } */
