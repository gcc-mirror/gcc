/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model -fno-signaling-nans" } */

#include <stdint-gcc.h>

#ifndef FN
#define FN(X) __builtin_fmax##X
#endif

#define DEF_LOOP(FN, TYPE, PRED_TYPE, NAME, CONST)	\
  void __attribute__ ((noipa))				\
  test_##TYPE##_##NAME (TYPE *__restrict x,		\
			TYPE *__restrict y,		\
			PRED_TYPE *__restrict pred,	\
			int n)				\
  {							\
    for (int i = 0; i < n; ++i)				\
      x[i] = pred[i] != 1 ? FN (y[i], CONST) : 0;	\
  }

#define TEST_TYPE(T, FN, TYPE, PRED_TYPE) \
  T (FN, TYPE, PRED_TYPE, zero, 0) \
  T (FN, TYPE, PRED_TYPE, one, 1) \
  T (FN, TYPE, PRED_TYPE, two, 2)

#define TEST_ALL(T) \
  TEST_TYPE (T, FN (f16), _Float16, int16_t)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {vfmax\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 3 } } */
/* { dg-final { scan-assembler-not {vfmax\.vf\s+v[0-9]+,v[0-9]+,fa[0-9],v0.t} } } */
/* { dg-final { scan-assembler-not {\tvf?merge\.v[vxi]m\t} } } */
