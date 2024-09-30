/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include <stdint-gcc.h>

#define DEF_LOOP(TYPE, PRED_TYPE, NAME, CONST)		\
  void __attribute__ ((noipa))				\
  test_##TYPE##_##NAME (TYPE *__restrict x,		\
			TYPE *__restrict y,		\
			PRED_TYPE *__restrict pred,	\
			int n)				\
  {							\
    for (int i = 0; i < n; ++i)				\
      x[i] = pred[i] != 1 ? y[i] + (TYPE) CONST : 0;	\
  }

#define TEST_TYPE(T, TYPE, PRED_TYPE) \
  T (TYPE, PRED_TYPE, half, 0.5) \
  T (TYPE, PRED_TYPE, one, 1.0) \
  T (TYPE, PRED_TYPE, two, 2.0) \
  T (TYPE, PRED_TYPE, minus_half, -0.5) \
  T (TYPE, PRED_TYPE, minus_one, -1.0) \
  T (TYPE, PRED_TYPE, minus_two, -2.0)

#define TEST_ALL(T) \
  TEST_TYPE (T, _Float16, int16_t) \
  TEST_TYPE (T, float, int32_t) \
  TEST_TYPE (T, double, int64_t)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {vfadd\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 0 } } */
/* { dg-final { scan-assembler-times {vfadd\.vf\s+v[0-9]+,v[0-9]+,fa[0-9],v0.t} 18 } } */
/* { dg-final { scan-assembler-not {\tvf?merge\.v[vxi]m\t} } } */
