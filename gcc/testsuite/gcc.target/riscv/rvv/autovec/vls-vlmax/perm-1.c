/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv -mabi=lp64d" } */

#include "perm.h"

#define MASK_2(X, Y) 1, 1
#define MASK_4(X, Y) MASK_2 (X, Y), MASK_2 (X + 2, Y)
#define MASK_8(X, Y) MASK_4 (X, Y), MASK_4 (X + 4, Y)
#define MASK_16(X, Y) MASK_8 (X, Y), MASK_8 (X + 8, Y)
#define MASK_32(X, Y) MASK_16 (X, Y), MASK_16 (X + 16, Y)
#define MASK_64(X, Y) MASK_32 (X, Y), MASK_32 (X + 32, Y)
#define MASK_128(X, Y) MASK_64 (X, Y), MASK_64 (X + 64, Y)

#define PERMUTE(TYPE, NUNITS)                                                  \
  __attribute__ ((noipa)) void permute_##TYPE (TYPE values1, TYPE values2,     \
					       TYPE *out)                      \
  {                                                                            \
    TYPE v                                                                     \
      = __builtin_shufflevector (values1, values2, MASK_##NUNITS (0, NUNITS)); \
    *(TYPE *) out = v;                                                         \
  }

#define TEST_ALL(T)                                                            \
  T (vnx2qi, 2)                                                                \
  T (vnx4qi, 4)                                                                \
  T (vnx8qi, 8)                                                                \
  T (vnx16qi, 16)                                                              \
  T (vnx32qi, 32)                                                              \
  T (vnx64qi, 64)                                                              \
  T (vnx128qi, 128)                                                            \
  T (vnx2hi, 2)                                                                \
  T (vnx4hi, 4)                                                                \
  T (vnx8hi, 8)                                                                \
  T (vnx16hi, 16)                                                              \
  T (vnx32hi, 32)                                                              \
  T (vnx64hi, 64)                                                              \
  T (vnx2si, 2)                                                                \
  T (vnx4si, 4)                                                                \
  T (vnx8si, 8)                                                                \
  T (vnx16si, 16)                                                              \
  T (vnx32si, 32)                                                              \
  T (vnx2di, 2)                                                                \
  T (vnx4di, 4)                                                                \
  T (vnx8di, 8)                                                                \
  T (vnx16di, 16)                                                              \
  T (vnx2sf, 2)                                                                \
  T (vnx4sf, 4)                                                                \
  T (vnx8sf, 8)                                                                \
  T (vnx16sf, 16)                                                              \
  T (vnx32sf, 32)                                                              \
  T (vnx2df, 2)                                                                \
  T (vnx4df, 4)                                                                \
  T (vnx8df, 8)                                                                \
  T (vnx16df, 16)

TEST_ALL (PERMUTE)

/* { dg-final { scan-assembler-times {vrgather\.vi\tv[0-9]+,\s*v[0-9]+,\s*1} 31 } } */
