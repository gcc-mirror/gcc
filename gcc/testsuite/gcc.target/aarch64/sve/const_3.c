/* { dg-do compile } */
/* { dg-options "-O3" } */

#include <stdint.h>

#define TEST(TYPE, CONST)			\
  void						\
  set_##TYPE (TYPE *dst, int count)		\
  {						\
    for (int i = 0; i < count; ++i)		\
      dst[i] = CONST;				\
  }

TEST (uint16_t, 0x1234)
TEST (uint32_t, 0x1234)
TEST (uint64_t, 0x1234)

/* { dg-final { scan-assembler {\tmov\t(w[0-9]+), 4660\n[^:]*\tmov\tz[0-9]+\.h, \1\n} } } */
/* { dg-final { scan-assembler {\tmov\t(w[0-9]+), 4660\n[^:]*\tmov\tz[0-9]+\.s, \1\n} } } */
/* { dg-final { scan-assembler {\tmov\t(x[0-9]+), 4660\n[^:]*\tmov\tz[0-9]+\.d, \1\n} } } */
