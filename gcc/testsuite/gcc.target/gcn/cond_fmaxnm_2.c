/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math -dp" } */
/* The 'scan-assembler' directives are specific to 64-lane vectors.
   { dg-additional-options --param=gcn-preferred-vectorization-factor=64 } */

#include <stdint.h>

#ifndef FN
#define FN(X) __builtin_fmax##X
#endif

#define DEF_LOOP(FN, TYPE, NAME, CONST)                                        \
  void __attribute__ ((noipa))                                                 \
  test_##TYPE##_##NAME (TYPE *__restrict x, TYPE *__restrict y,                \
			TYPE *__restrict z, int n)                             \
  {                                                                            \
    for (int i = 0; i < n; ++i)                                                \
      x[i] = y[i] < 8 ? FN (z[i], CONST) : y[i];                               \
  }

#define TEST_TYPE(T, FN, TYPE)                                                 \
  T (FN, TYPE, zero, 0)                                                        \
  T (FN, TYPE, one, 1)                                                         \
  T (FN, TYPE, two, 2)

#define TEST_ALL(T)                                                            \
  TEST_TYPE (T, FN (f32), float)                                               \
  TEST_TYPE (T, FN (f64), double)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {smaxv64sf3_exec} 3 } } */
/* { dg-final { scan-assembler-times {smaxv64df3_exec} 3 } } */

/* { dg-final { scan-assembler-not {\tv_writelane_b32\tv[0-9]+, vcc_..} } } */
