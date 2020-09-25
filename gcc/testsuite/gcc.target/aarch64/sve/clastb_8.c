/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=256 --save-temps" } */

#include <stdint.h>

#define TEST_TYPE(TYPE) \
  void \
  test_##TYPE (TYPE *ptr, TYPE *a, TYPE *b, TYPE min_v) \
  { \
    TYPE last = *ptr; \
    for (int i = 0; i < 1024; i++) \
      if (a[i] < min_v) \
	last = b[i]; \
    *ptr = last; \
  }

TEST_TYPE (uint8_t);
TEST_TYPE (uint16_t);
TEST_TYPE (uint32_t);
TEST_TYPE (uint64_t);

/* { dg-final { scan-assembler {\tclastb\t(b[0-9]+), p[0-7], \1, z[0-9]+\.b\n} } } */
/* { dg-final { scan-assembler {\tclastb\t(h[0-9]+), p[0-7], \1, z[0-9]+\.h\n} } } */
/* { dg-final { scan-assembler {\tclastb\t(s[0-9]+), p[0-7], \1, z[0-9]+\.s\n} } } */
/* { dg-final { scan-assembler {\tclastb\t(d[0-9]+), p[0-7], \1, z[0-9]+\.d\n} } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-9]+\.b,} 4 } } */
