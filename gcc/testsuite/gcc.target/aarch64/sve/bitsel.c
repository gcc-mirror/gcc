/* { dg-options "-O2 -mcpu=neoverse-v2 --param aarch64-autovec-preference=asimd-only" } */

#include <stdint.h>

#define OPNBSL(x,y,z) (~(((x) & (z)) | ((y) & ~(z))))
#define OPBSL1N(x,y,z) ((~(x) & (z)) | ((y) & ~(z)))
#define OPBSL2N(x,y,z) (((x) & (z)) | (~(y) & ~(z)))

#define N 1024

#define TYPE(N) int##N##_t

#define TEST(SIZE, OP, SUFFIX)                                  \
void __attribute__ ((noinline, noclone))                        \
f_##SIZE##_##SUFFIX                                             \
  (TYPE(SIZE) *restrict a, TYPE(SIZE) *restrict b,              \
   TYPE(SIZE) *restrict c, TYPE(SIZE) *restrict d)              \
{                                                               \
  for (int i = 0; i < N; i++)                                   \
    a[i] = OP (b[i], c[i], d[i]);                               \
}

#define TEST_ALL(SIZE)                                          \
  TEST(SIZE, OPNBSL, nbsl)                                      \
  TEST(SIZE, OPBSL1N, bsl1n)                                    \
  TEST(SIZE, OPBSL2N, bsl2n)

TEST_ALL(8);
TEST_ALL(16);
TEST_ALL(32);
TEST_ALL(64);

/* { dg-final { scan-assembler-times {\tnbsl\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 4 } } */
/* { dg-final { scan-assembler-times {\tbsl1n\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 4 } } */
/* { dg-final { scan-assembler-times {\tbsl2n\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 4 } } */