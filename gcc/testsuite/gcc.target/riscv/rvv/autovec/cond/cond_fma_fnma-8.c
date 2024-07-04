/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model -ffast-math" } */

#include <stdint-gcc.h>

#define DEF_LOOP(TYPE, NAME, OP, CONST)				\
  void __attribute__ ((noipa))					\
  test_##TYPE##_##NAME##_##CONST (TYPE *__restrict r, 		\
				  TYPE *__restrict a,		\
				  TYPE *__restrict b,		\
				  TYPE *__restrict pred, int n)	\
  {								\
    for (int i = 0; i < n; ++i)					\
      r[i] = pred[i] != 1 ? a[i] OP b[i] * -CONST : a[i];	\
  }

#define TEST_COUNT(T, TYPE, CONST) \
  T (TYPE, add, +, CONST) \
  T (TYPE, sub, -, CONST)

#define TEST_TYPE(T, TYPE, CONST) \
  TEST_COUNT (T, TYPE, 2) \
  TEST_COUNT (T, TYPE, 4) \
  TEST_COUNT (T, TYPE, CONST)

#define TEST_ALL(T) \
  TEST_TYPE (T, uint8_t, 0x80) \
  TEST_TYPE (T, uint16_t, 0x8000) \
  TEST_TYPE (T, uint32_t, 0x80000000) \
  TEST_TYPE (T, uint64_t, 0x8000000000000000ULL)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {vmacc\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 12 } } */
/* { dg-final { scan-assembler-times {vnmsac\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 12 } } */
/* { dg-final { scan-assembler-not {\tvf?merge\.v[vxi]m\t} } } */
