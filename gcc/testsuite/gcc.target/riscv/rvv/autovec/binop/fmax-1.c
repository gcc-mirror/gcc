/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=scalable --param vect-epilogues-nomask=0 -fno-signaling-nans" } */

#include <stdint-gcc.h>

#ifndef FN
#define FN(X) __builtin_fmax##X
#endif

#define DEF_LOOP(FN, SUFFIX, TYPE)                                             \
  void __attribute__ ((noipa))                                                 \
  test_##TYPE (TYPE *__restrict x, TYPE *__restrict y, int n)                  \
  {                                                                            \
    for (int i = 0; i < n; ++i)                                                \
      x[i] = FN (SUFFIX) (x[i], y[i]);                                         \
  }

#define TEST_ALL(T)                                                            \
  T (FN, f, float)                                                             \
  T (FN, , double)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {vfmax\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+} 2 } } */
