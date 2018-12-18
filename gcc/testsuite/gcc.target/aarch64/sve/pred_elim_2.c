/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define TEST_OP(NAME, TYPE, OP)					\
  void								\
  NAME##_##TYPE (TYPE *restrict a, TYPE *restrict b, int n)	\
  {								\
    for (int i = 0; i < n; ++i)					\
      a[i] = b[i] OP;						\
  }

#define TEST_TYPE(TYPE) \
  TEST_OP (shl, TYPE, << 6) \
  TEST_OP (shr, TYPE, >> 6) \
  TEST_OP (mult, TYPE, * 0x2b)

TEST_TYPE (int8_t)
TEST_TYPE (int16_t)
TEST_TYPE (int32_t)
TEST_TYPE (int64_t)
TEST_TYPE (uint8_t)
TEST_TYPE (uint16_t)
TEST_TYPE (uint32_t)
TEST_TYPE (uint64_t)

/* { dg-final { scan-assembler-times {\tlsl\t} 8 } } */
/* { dg-final { scan-assembler-times {\tlsr\t} 4 } } */
/* { dg-final { scan-assembler-times {\tasr\t} 4 } } */
/* { dg-final { scan-assembler-times {\tmul\t} 8 } } */
/* { dg-final { scan-assembler-not {\tptrue\t} } } */
