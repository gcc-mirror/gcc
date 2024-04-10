/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include <stdint-gcc.h>

#define DEF_LOOP(TYPE, NAME, OP)					\
  void __attribute__ ((noipa))						\
  test_##TYPE##_##NAME (TYPE *__restrict r, TYPE *__restrict a,		\
			TYPE *__restrict b, TYPE *__restrict c, int n)	\
  {									\
    for (int i = 0; i < n; ++i)						\
      r[i] = a[i] > 20 ? b[i] OP c[i] : c[i];				\
  }

#define TEST_TYPE(T, TYPE) \
  T (TYPE, shl, <<) \
  T (TYPE, shr, >>)

#define TEST_ALL(T) \
  TEST_TYPE (T, int32_t) \
  TEST_TYPE (T, uint32_t)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {vsll\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsra\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsrl\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 1 } } */
/* { dg-final { scan-assembler-not {\tvf?merge\.v[vxi]m\t} } } */
