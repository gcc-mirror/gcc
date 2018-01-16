/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O3 -msve-vector-bits=256 --save-temps" } */

#include <stdint.h>

#define DUP4(X) X, X, X, X
#define DUP8(X) DUP4 (X), DUP4 (X)
#define DUP16(X) DUP8 (X), DUP8 (X)
#define DUP32(X) DUP16 (X), DUP16 (X)

typedef uint8_t vuint8_t __attribute__ ((vector_size (32)));
typedef uint16_t vuint16_t __attribute__ ((vector_size (32)));
typedef uint32_t vuint32_t __attribute__ ((vector_size (32)));
typedef uint64_t vuint64_t __attribute__ ((vector_size (32)));

#define TEST(TYPE, NAME, INIT)					\
  void								\
  NAME##_##TYPE (TYPE *dest, __typeof__(dest[0][0]) *ptr)	\
  {								\
    TYPE x = { INIT };						\
    *dest = x;							\
  }

#define TEST_GROUP(TYPE, NAME, DUP)		\
  TEST (TYPE, NAME_##m1, DUP (ptr[-1]))		\
  TEST (TYPE, NAME_##0, DUP (ptr[0]))		\
  TEST (TYPE, NAME_##63, DUP (ptr[63]))		\
  TEST (TYPE, NAME_##64, DUP (ptr[64]))

TEST_GROUP (vuint8_t, t8, DUP32)
TEST_GROUP (vuint16_t, t16, DUP16)
TEST_GROUP (vuint32_t, t16, DUP8)
TEST_GROUP (vuint64_t, t16, DUP4)

/* { dg-final { scan-assembler-not {\tld1rb\tz[0-9]+\.b, p[0-7]/z, \[x1, -1\]\n} } } */
/* { dg-final { scan-assembler {\tld1rb\tz[0-9]+\.b, p[0-7]/z, \[x1\]\n} } } */
/* { dg-final { scan-assembler {\tld1rb\tz[0-9]+\.b, p[0-7]/z, \[x1, 63\]\n} } } */
/* { dg-final { scan-assembler-not {\tld1rb\tz[0-9]+\.b, p[0-7]/z, \[x1, 64\]\n} } } */

/* { dg-final { scan-assembler-not {\tld1rh\tz[0-9]+\.h, p[0-7]/z, \[x1, -1\]\n} } } */
/* { dg-final { scan-assembler {\tld1rh\tz[0-9]+\.h, p[0-7]/z, \[x1\]\n} } } */
/* { dg-final { scan-assembler {\tld1rh\tz[0-9]+\.h, p[0-7]/z, \[x1, 126\]\n} } } */
/* { dg-final { scan-assembler-not {\tld1rh\tz[0-9]+\.h, p[0-7]/z, \[x1, 128\]\n} } } */

/* { dg-final { scan-assembler-not {\tld1rw\tz[0-9]+\.s, p[0-7]/z, \[x1, -1\]\n} } } */
/* { dg-final { scan-assembler {\tld1rw\tz[0-9]+\.s, p[0-7]/z, \[x1\]\n} } } */
/* { dg-final { scan-assembler {\tld1rw\tz[0-9]+\.s, p[0-7]/z, \[x1, 252\]\n} } } */
/* { dg-final { scan-assembler-not {\tld1rw\tz[0-9]+\.s, p[0-7]/z, \[x1, 256\]\n} } } */

/* { dg-final { scan-assembler-not {\tld1rd\tz[0-9]+\.d, p[0-7]/z, \[x1, -1\]\n} } } */
/* { dg-final { scan-assembler {\tld1rd\tz[0-9]+\.d, p[0-7]/z, \[x1\]\n} } } */
/* { dg-final { scan-assembler {\tld1rd\tz[0-9]+\.d, p[0-7]/z, \[x1, 504\]\n} } } */
/* { dg-final { scan-assembler-not {\tld1rd\tz[0-9]+\.d, p[0-7]/z, \[x1, 512\]\n} } } */
