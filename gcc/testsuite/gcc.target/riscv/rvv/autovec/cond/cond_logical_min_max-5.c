/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include <stdint-gcc.h>

#define bit_and(A, B) ((A) & (B))
#define bit_or(A, B) ((A) | (B))
#define bit_xor(A, B) ((A) ^ (B))
#define bit_bic(A, B) ((A) & ~(B))
#define min(A, B) ((A) <= (B) ? (A) : (B))
#define max(A, B) ((A) >= (B) ? (A) : (B))

#define DEF_LOOP(TYPE, OP)                                                     \
  void __attribute__ ((noinline, noclone))                                     \
  test_##TYPE##_##OP (TYPE *__restrict r, TYPE *__restrict a,                  \
		      TYPE *__restrict b, TYPE *__restrict c, int n)           \
  {                                                                            \
    for (int i = 0; i < n; ++i)                                                \
      r[i] = a[i] < 20 ? OP (b[i], c[i]) : 0;                                  \
  }

#define TEST_TYPE(T, TYPE)                                                     \
  T (TYPE, bit_and)                                                            \
  T (TYPE, bit_or)                                                             \
  T (TYPE, bit_xor)                                                            \
  T (TYPE, bit_bic)                                                            \
  T (TYPE, min)                                                                \
  T (TYPE, max)

#define TEST_ALL(T)                                                            \
  TEST_TYPE (T, int8_t)                                                        \
  TEST_TYPE (T, uint8_t)                                                       \
  TEST_TYPE (T, int16_t)                                                       \
  TEST_TYPE (T, uint16_t)                                                      \
  TEST_TYPE (T, int32_t)                                                       \
  TEST_TYPE (T, uint32_t)                                                      \
  TEST_TYPE (T, int64_t)                                                       \
  TEST_TYPE (T, uint64_t)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {vnot\.v\s+v[0-9]+,v[0-9]+} 8 } } */
/* { dg-final { scan-assembler-times {vand\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 16 } } */
/* { dg-final { scan-assembler-times {vor\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 8 } } */
/* { dg-final { scan-assembler-times {vxor\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 8 } } */
/* { dg-final { scan-assembler-times {vmin\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 4 } } */
/* { dg-final { scan-assembler-times {vmax\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 4 } } */
/* { dg-final { scan-assembler-times {vminu\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 4 } } */
/* { dg-final { scan-assembler-times {vmaxu\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 4 } } */
/* { dg-final { scan-assembler-not {\tvf?merge\.v[vxi]m\t} } } */
