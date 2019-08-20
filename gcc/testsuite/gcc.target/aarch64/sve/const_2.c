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

TEST (uint16_t, 129)
TEST (uint32_t, 129)
TEST (uint64_t, 129)

/* { dg-final { scan-assembler {\tmovi\tv([0-9]+)\.8h, 0x81\n[^:]*\tdup\tz[0-9]+\.q, z\1\.q\[0\]\n} } } */
/* { dg-final { scan-assembler {\tmovi\tv([0-9]+)\.4s, 0x81\n[^:]*\tdup\tz[0-9]+\.q, z\1\.q\[0\]\n} } } */
/* { dg-final { scan-assembler {\tmov\t(x[0-9]+), 129\n[^:]*\tmov\tz[0-9]+\.d, \1\n} } } */
