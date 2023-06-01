/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv -mabi=lp64d" } */

#include "perm.h"

#define MASK_2(X, Y) 55, 55
#define MASK_4(X, Y) MASK_2 (X, Y), MASK_2 (X + 2, Y)
#define MASK_8(X, Y) MASK_4 (X, Y), MASK_4 (X + 4, Y)
#define MASK_16(X, Y) MASK_8 (X, Y), MASK_8 (X + 8, Y)
#define MASK_32(X, Y) MASK_16 (X, Y), MASK_16 (X + 16, Y)
#define MASK_64(X, Y) MASK_32 (X, Y), MASK_32 (X + 32, Y)
#define MASK_128(X, Y) MASK_64 (X, Y), MASK_64 (X + 64, Y)

#define PERMUTE(TYPE, NUNITS)                                                  \
  void permute_##TYPE (TYPE values1, TYPE values2, TYPE *out)                  \
  {                                                                            \
    TYPE v                                                                     \
      = __builtin_shufflevector (values1, values2, MASK_##NUNITS (0, NUNITS)); \
    *(TYPE *) out = v;                                                         \
  }

#define TEST_ALL(T)                                                            \
  T (vnx64qi, 64)                                                              \
  T (vnx128qi, 128)                                                            \
  T (vnx64hi, 64)

TEST_ALL (PERMUTE)

/* { dg-final { scan-assembler-times {vrgather\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 3 } } */
